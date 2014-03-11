# Erlang NCSA Client

## API

``` erlang
ok = icinga:submit_async(warning, "service description", "message text").

ok = icinga:submit_sync(warning, "service description", "message text").
```

## Application config

| Option          | Type                            | Default           |
|:--------------- |:------------------------------- |:----------------- |
| server_hostname | string()                        | `"localhost"`     |
| server_port     | integer()                       | `9042`            |
| server_password | string()                        | `"chang3m3`       |
| server_timeout  | integer()                       | `5000`            |
| client_hostname | string()                        | `"localhost"`     |
