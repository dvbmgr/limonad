# limonad
limonad is a web interface for Darcs

License
=======

Distributed under the BSD license. See LICENSE.

Authors 
=======

David Baumgartner <ch.davidbaumgartner@gmail.com>

How to use
==========

Developpement
-------------

The simplest way to start *limonad* as a server is to type `make run`.

This will make the server listen to 0.0.0.0:8080.

Production
----------

Firstly open a screen.

```
$ screen -R limonad

# In the screen, type 
make run
```

You can configure Nginx this way.

Copy the following content in `/etc/nginx/sites-enabled/limonad`.

```
server {
	listen 80;
	server_name limonad.local;

	location / {
		proxy_pass http://localhost:8080;
	}
}
```

Apache version is quite similar. This requires `mod_proxy`

```
Listen 80
<VirtualHost *:80>
    ServerName limonad.local
    ProxyPass / http://localhost:8080/
</VirtualHost>
```
