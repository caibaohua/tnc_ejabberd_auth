tnc_ejabberd_auth
=================

This tool is used to work as ejabberd authentication module.

## Install
1. Run ./install.sh
2. Copy enbin/*.bean files into path/to/ejabberd/ebin/ (such as /opt/local/lib/ejabberd/ebin/ etc)

## Settings
Modify ejabberd.cfg and,

{auth_method, tnc}.
{communities, [
        {
                "localhost", [
                        {service, "redis"},
                        {hosts, [{"moleman-flirten", 6379, 1, 1}]},
                        {retry_timeout, 30000}
                ]
        },
        {
                "xmpp.flirten.lab", [
                        {service, "memcache"},
                        {hosts, [{"10.60.1.237", 11211, 1}, {"10.60.1.237", 11210, 1}]},
                        {retry_timeout, 30000},
                        {backoff_interval, 30000},
                        {max_retry_backoff, 1800000}
                ]
        }
]}.
