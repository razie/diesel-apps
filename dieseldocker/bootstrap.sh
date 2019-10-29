#!/bin/sh

## Script to bootstrap a local hosted instance of DieselApps

# wait for it to be up
printf "Waiting for diesel to come up..."
until $(curl --fail --output /dev/null --silent --head 'http://diesel:9000/admin/ping'); do
printf "."
sleep 5
done
printf "Done!\n\n"

## server up - configure it
curl -i -H "Content-Type: application/x-www-form-urlencoded" 'http://diesel:9000/spec/importDbSync' \
-X POST \
-d 'source=www.dieselapps.com&realm=MYREALM&email=MYEMAIL&pwd=MYPASSWORD'


