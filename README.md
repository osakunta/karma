# KARMA

## `supervisord.conf`

```ini
[program:karma]
command=/usr/local/bin/karma-server
autorestart=true
user=daemon
environment=PORT="8081",POSTGRES_URL=...,ACTION_URL=/karma
```

## `apache.conf`

```xml
ProxyPass        /karma http://localhost:8081
ProxyPassReverse /karma http://localhost:8081
```

## Actions

- tyhjensin tiskikoneen
- täytin tiskikoneen
- tiskasin
- siivosin porin huoneen pöydät
- siivosin kirjaston ja rauman huoneen pöydät