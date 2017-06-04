# Kolo

## Build

```
stack build
```

## Generate Self-Signed SSL Cert

```
openssl req -x509 -newkey rsa:4096 -keyout server.key -out server.crt -days 365 -nodes
```

And answer all the questions. Just pressing enter to default everything works fine.

## Run Server

```
stack exec kolo-exe
```

## Use OpenSSL Client to Connect

```
openssl s_client -quiet -connect localhost:7779
```

