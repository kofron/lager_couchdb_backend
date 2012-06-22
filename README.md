lager_couchdb_backend
=====================

A backend for lager using couchdb as persistence and couchbeam for the CouchDB interface.
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