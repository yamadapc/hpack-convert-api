heroku:
	heroku create --buildpack https://github.com/mfine/heroku-buildpack-stack.git hpack-convert
	git push heroku master
	make set-host

set-host:
	heroku config:set HOST=`heroku info -s | grep web-url | cut -d= -f2`
