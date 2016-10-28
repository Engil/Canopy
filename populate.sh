#!/bin/sh

TARGET=$1

if [ -x `which npm` ]; then
    if [ -d $1 ]; then
        mkdir -p $1/static/css $1/static/js
        npm install
        browserify assets/js/main.js -o $1/static/js/canopy.js
        lessc assets/less/style.less $1/static/css/style.css --source-map-map-inline --strict-imports
        cp node_modules/bootstrap/dist/css/bootstrap.min.css $1/static/css/bootstrap.min.css
        cp node_modules/highlight.js/styles/grayscale.css $1/static/css/highlight.css
        echo "now go to $1 and git add static && git commit -m . && git push"
    else
        echo "please run as 'populate.sh data_repository'"
    fi
else
    echo "npm not found, please unpack assets/assets_generated.tar.gz to your data repository"
fi
