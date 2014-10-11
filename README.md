
Wobsurv
=======

A simple and highly performant HTTP file server.


Noteable features:
------------------

* Based on streaming. Produces the response while the request is still coming. It doesn't waste resources on incorrect or malicious requests by dismissing them right away. It is very gentle with memory.

* Has a configurable limit of simultaneous connections. All exceeding requests get rejected with a "Service Unavailable" status with code 503.


---

[![Build Status](https://travis-ci.org/nikita-volkov/wobsurv.svg)](https://travis-ci.org/nikita-volkov/wobsurv)
