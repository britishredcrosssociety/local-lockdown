<img src='figures/title.png' align="left" height ="60"/><img src='figures/brc-logo.png' align="right" height ="65"/>

<br/><br/>

[![Contributor Covenant](https://img.shields.io/badge/Contributor%20Covenant-v2.0%20adopted-ff69b4.svg)](code_of_conduct.md)

## Overview
R Shiny dashboard for visualising information related to COVID-19 Test &amp; Trace and local lockdowns. Access the dashboard [here](https://vcsep-local-lockdown-app.azurewebsites.net).

## Saving State

> **tl;dr** - treat the application as stateless. Any files written to the disk may not be there in future.

Due in part to how this application is deployed, as a Docker container in [Azure App Service](https://azure.microsoft.com/en-gb/services/app-service/),
and based on the initial requirements, no dynamic state can be stored in this instance. We are on the dynamic plan, which
means we cannot guarantee that any state stored to disk will be there on a future run.

As a workflow, Azure App Services work like this:

1. User goes to URL
2. App checks if server is already running
    1. If running, serve application
    2. Else, start application and download latest Docker image
3. Turn off application if no new requests come in during timeout period

The key here is the timeout period (< 5 mins - not configurable). If a new request comes in during that time, it keeps
the application running and any statefiles on there. If it's not running, it will be a new instance of the application
without any of the files written to the disk during any previous instance.

As a future improvement, we can map to blob storage account to the instance where things would be written, but that is
not currently in-scope.

## Deployment

[Website](https://vcsep-local-lockdown-app.azurewebsites.net)

> **tl;dr** - anything pushed to the `master` branch gets deployed

A Dockerfile is provided for easy deployment, which is managed centrally. There are [GitHub Actions](https://github.com/features/actions)
to build a Docker image and publish it to the BRC/VCSEP registry. The action of building and publishing a new image
triggers the server to download that image and run it on the server.

### Build the Docker image

> See [Docker website](https://docs.docker.com/engine/install/) for installation instructions

```bash
docker build -t local-lockdown .
```

Due to the size of R dependencies, this will take ~15 minutes to build and take up around 2.2GB in space.

### Run the Docker container

Once you've built the image, you can use Docker to run it

```bash
docker run -it --rm -p 8080:3838 local-lockdown
```

Once that's running, you can visit [localhost:8080](http://localhost:8080) to
view the application.

### todos

- [ ] Add tests to ensure that the Docker image runs correctly
- [ ] Automate dependency installation using `requirements.txt` format (or similar)
- [ ] Establish better fix for error installing [tidyr@1.1.1](https://stackoverflow.com/questions/63348135/error-installing-tidyr-on-ubuntu-18-04-r-4-0-2)
- [ ] Run Actions on all branches, only publishing on `master`
- [ ] Add mapping to store state files in blob storage

## Contributing
To contribute to this project, please follow [GitHub Flow](https://guides.github.com/introduction/flow/) when submitting changes.

> Please note that this project is released with a Contributor Code of Conduct. By participating in this project you agree to abide by its terms.

## Getting help
If you encounter a clear bug, please file a minimal reproducible example in [issues](https://github.com/britishredcrosssociety/local-lockdown/issues).

