{{=<% %>=}}
#!/bin/bash

export RELX_REPLACE_OS_VARS=true
export NODE_NAME="${NODE_NAME:-<% name %>}"
export NODE_COOKIE="${NODE_COOKIE:-<% name %>_cookie}"
export HTTP_PORT="${HTTP_PORT:-8000}"
export REDIS_HOST="${REDIS_HOST:-127.0.0.1}"
export REDIS_PORT="${REDIS_PORT:-6379}"
export REDIS_DB="${REDIS_DB:-0}"
# export REDIS_PASS=""
export XMPP_HOST="${XMPP_HOST:-127.0.0.1}"
export XMPP_PORT="${XMPP_PORT:-8888}"
export XMPP_DOMAIN="${XMPP_DOMAIN:-comp.localhost}"
export XMPP_SECRET="${XMPP_SECRET:-secret}"

$(dirname $0)/<% name %> $@
<%={{ }}=%>
