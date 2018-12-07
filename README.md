# FIC Scripts

[![Build Status](https://travis-ci.org/Fellowship-For-Intentional-Community/Wordpress-Scripts.svg?branch=master)](https://travis-ci.org/Fellowship-For-Intentional-Community/Wordpress-Scripts)


This is an experimental repository containing export scripts for our Wordpress
site, written in Haskell.

You should make an `env.sh` with the following content:

```sh
export DB_USER="<mysqluser>" DB_PASS="<mysqlpass>" DB_NAME="<database>"
export MAPS_API_KEY="<googe maps api key>"
```

Then you can build/run the scripts with:

```sh
sudo apt-get install libmariadbclient-dev libpcre3-dev  # For Debian/Ubuntu
stack build
source ./env.sh
stack exec directory-fix-coordinates -- --delete-private
```

Some scripts have CLI arguments/flags to change their actions, use the `--help`
flag on any script to list their options:

```sh
$ stack exec subscriber-export -- --help
Subscriptions - Subscriber Export

subscriber-export [OPTIONS]
  Export the name & email of all active subscribers.

Common flags:
  -p --products=PRODUCT_ID,...
  -v --variations=VARIATION_ID,...
  -? --help                         Display help message
  -V --version                      Print version information

Any subscriptions with a matching product or variation will be included.

Passing no product or variation IDs will export all active subscriptions.
```


## License

GPL-3.0
