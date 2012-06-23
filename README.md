lager_couchdb_backend
=====================

A backend for lager using couchdb as persistence.  There are no external dependencies - httpc is used
in inets as a simple POST API.  It is in its infancy but is stable, simple, and easy.

The config file is simple and looks like this:

[{lager, [
  {handlers, [
    {lager_couchdb_backend, [
      {name, "lager_couchdb_backend"},
      {level, debug},
      {host, "YOURHOST"},
      {port, 5984}
    ]}
  ]}
]}].