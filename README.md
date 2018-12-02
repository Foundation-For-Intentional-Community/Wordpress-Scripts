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

Some scripts have CLI arguments to change their actions, check the scripts and
use the `--help` flag to list options.


## License

GPL-3.0
