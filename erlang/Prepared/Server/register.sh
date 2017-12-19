#!/bin/bash

curl -X POST \
  http://localhost:2345/register \
  -H 'Cache-Control: no-cache' \
  -H 'Content-Type: application/x-www-form-urlencoded' \
  -H 'Postman-Token: 0e3f9dcb-e3fc-ce90-d563-b1f30cef2ef4' \
  -d 'host='$1'&port='$2'&topic='$3