# Database Settings ----------
DB_NAME=testdb
DB_HOST=localhost
DB_PORT=5432
DB_USER=main_user
DB_PASSWORD=password
# ----------------------------

TEMP_FILE=$(mktemp)

cat $1 |
sed -e "s/<|DB_NAME|>/${DB_NAME}/g" |
sed -e "s/<|DB_HOST|>/${DB_HOST}/g" |
sed -e "s/<|DB_PORT|>/${DB_PORT}/g" |
sed -e "s/<|DB_USER|>/${DB_USER}/g" |
sed -e "s/<|DB_PASSWORD|>/${DB_PASSWORD}/g" > ${TEMP_FILE}

mv ${TEMP_FILE} $1