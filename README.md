# wordbank-shiny

Shiny apps for Wordbank.

Apps are hosted on our Posit Connect instance (hosted on EC2) at `https://wordbank-shiny.com`. To deploy them, you'll need a user account with publisher permission on Connect. You can obtain user credentials by going to that address in browser and clicking on "Sign Up", then asking an admin to give you publisher permissions.

## Editing an existing app
- Clone this repository.
- Open the RStudio project for one of the apps in the `apps/` directory.
- Make whatever changes you're making to `server.R`, `ui.R`, and `global.R`. Remember to iteratively ensure the app runs locally with `runApp()`.
- Make sure that any other files that the apps needs (e.g. if you're embedding an image) are inside *that apps's* directory.
- If you've made any changes to which packages are installed, run `renv::snapshot()` to update the app's `renv.lock` file.

## Deploying an existing app
- In the app's directory, run `rsconnect::writeManifest()` and then `rsconnect::deployApp()`.
- If this is your first time deploying this app, you'll need to provide its address (`https://wordbank-shiny.com`), and if it's your first deploying any apps, you'll need to provude your user credentials.
- Once the app is deployed, it should open in a browser -- make sure its running succesfully, and then you're done!

## Adding a new app
- Create a new directory within `apps/` for your app, containing `server.R`, `ui.R`, and any other files. Give it its own new RStudio project and renv environment. (The easiest way of doing this is in RStudio is New Project > New Directory > Shiny Application, checking "Use renv with this project").
- Copy the files `global.R` and `common.R` from another one of the apps.
- Develop the app!
- If developing the app necessitated any changes to `common.R`, propagate them to the `common.R` file in the root of the repo and then run `./scripts/update_common.sh`. Be careful to make sure these changes don't break other apps.
- Follow the same steps as above to deploy the app:
```
renv::snapshot()
rsconnect::writeManifest()
rsconnect::deployApp()
```
- Once the app is done deploying, open it in the Connect admin interface and under "Access", change "Sharing" to "Anyone - no login required", and under "Content URL", set "Path" to a short identifier for the app (ideally the same as the app's directory).
- In [this file in the wordbank repo](https://github.com/langcog/wordbank/blob/master/wordbank/static/json/analyses.json), add an entry for the app with its title, name (important: same as what you used for the path above), and thumbnail. Note that the json file has the apps structured into sections, and this structure is reflected in the boxes on the [Data page](https://wordbank.stanford.edu/data/) and in the navbar on [each app's page](https://wordbank.stanford.edu/data/?name=vocab_norms).
- Deploy wordbank to elastic beanstalk (`eb deploy wordbank-dev` and `eb deploy wordbank-prod` assuming your aws cli is installed + configured).

## Administering the Connect server
- For most changes, deploying apps with the above workflow is all that's needed and there's no need to ssh into the server. Direct server access is needed for things like if: something needs to be changed about the Connect configuration; a new Connect license needs to be activated; the system needs changes (e.g. new system-level dependencies); or the Connect instance needs to be restarted for whatever reason.
- See the [Connect docs](https://docs.posit.co/connect/admin/) for everything about configuration/management.
- Specifically, changing configuration involves changes to the `/etc/rstudio-connect/rstudio-connect.gcfg` file and activating a new license key is done with:
```
sudo /opt/rstudio-connect/bin/license-manager activate <LICENSE-KEY>
sudo /opt/rstudio-connect/bin/license-manager deactivate
sudo systemctl restart rstudio-connect
```
- If `https://wordbank-shiny.com` has a new SSL certificate, the certificate and private key files need to be placed in `/etc/rstudio-connect/`, as per the [setting in the configuration file](https://docs.posit.co/connect/admin/getting-started/local-install/initial-configuration/ssl-certificates/) being:
```
[HTTPS]
Listen = :443
Certificate = /etc/rstudio-connect/wordbank-shiny_com.crt
Key = /etc/rstudio-connect/private.key
```

## Switching hosts
- There are backup versions of all the apps deployed to shinyapps.io under the mcfrank@stanford.edu account. If the Connect server is down for whatever reason, the wordbank website can be use those apps instead.
- In [this file in the wordbank repo](https://github.com/langcog/wordbank/blob/master/wordbank/settings.py), change the variable `SHINY_SERVER_URL` to `"https://wordbank-shiny.com"` (or a different host if needed), then deploy wordbank to elastic beanstalk.

## Updating data
- The Shiny apps use wordbank data from a local database on the same EC2 instance that's running the Connect instance (because this makes data loading much faster). This database is a copy of the main wordbank database on rds.
- To update the database on this instance, a dump of the main database that you want to use needs to be put in s3 and then loaded here.
- To create a new version dump:
```
wbpath=/home/ubuntu/wordbank-versions/wordbank_$(date +%Y%m%d).sql
mysqldump -h wordbank2-prod.canyiscnpddk.us-west-2.rds.amazonaws.com -u wordbankadmin -p --databases wordbank --add-drop-database --result-file=${wbpath}
aws s3 cp ${wbpath} s3://wordbank-versioning
rm ${wbpath}
```
Then to load it into the local database:
```
aws s3 ls s3://wordbank-versioning
# pick desired version
wb_version=YOUR_VERSION_HERE
wget https://wordbank-versioning.s3.amazonaws.com/wordbank_${wb_version}.sql
sudo mysql < wordbank_${wb_version}.sql
```
- Most of the apps use the local database directly, with the exception of the `uni_lemmas` app, which has a cached dataset within it (`apps/uni_lemmas/all_prop_data.rds`). A new version of this data can be creating using the `apps/uni_lemmas/cache_all_prop_data.R` script.
