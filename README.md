# rAPImo

An R API to perform RABIMO calculations.

## How to start the API using Docker

Run the build.sh and run.sh scripts from your terminal, like this:

```
sh build.sh
sh run.sh
```

You can now access the API Swagger Documentation on your browser at:

http://127.0.0.1:8000/__docs__/

## Use HTTPS

Follow these instructions to build and run the app using HTTPS. 
These instructions are meant just for testing locally and with self-signed certificates.
In the future, the application will be hosted on the cloud and will use proper certificates.

1. Ensure you have Docker, Docker Compose and Nginx installed on your computer.
2. Create self-signed certificates with the following command:

```
openssl req -x509 -nodes -days 365 -newkey rsa:2048 -keyout ssl/privkey.pem -out ssl/fullchain.pem -subj "/CN=localhost"
```

3. Build and run the containers with the following command:

```
docker-compose up --build
```

You can test that both containers (nginx and rapimo) are up and running with this command:

```
docker ps
```

4. Test that the application is correctly running by visiting `https://127.0.0.1:443/__docs__/` on your browser.
Since you're using a self-signed certificate, your browser will likely show a warning about the certificate being untrusted. You can proceed to the site despite the warning for testing purposes.
