# find wb version to get
# listed here https://s3.console.aws.amazon.com/s3/buckets/wordbank-versioning?region=us-west-2&tab=objects
# or by running
# aws s3 ls s3://wordbank-versioning
wb_version=22072022

# download .sql file from s3
# aws s3 cp s3://wordbank-versioning/wordbank_${wordbank_version}.sql wordbank-versions/
wget https://wordbank-versioning.s3.amazonaws.com/wordbank-${wb_version}.sql -P wordbank-versions/

# set up .sql file into db
sudo mysql < wordbank-versions/wordbank-${wb_version}.sql
