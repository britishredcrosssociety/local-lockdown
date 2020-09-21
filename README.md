<img src='figures/title.png' align="left" height ="60"/><img src='figures/brc-logo.png' align="right" height ="65"/>

<br/><br/>

[![Contributor Covenant](https://img.shields.io/badge/Contributor%20Covenant-v2.0%20adopted-ff69b4.svg)](code_of_conduct.md)

## Overview
R Shiny dashboard for visualising information related to COVID-19 Test &amp; Trace and local lockdowns. Access the dashboard [here](https://vcsep-local-lockdown-app.azurewebsites.net).

## Deployment

[Website](https://vcsep-local-lockdown-app.azurewebsites.net)

> **tl;dr** - anything pushed to the `master` branch gets deployed

A Dockerfile is provided for easy deployment, which is managed centrally. There are [GitHub Actions](https://github.com/features/actions)
to build a Docker image and publish it to the BRC/VCSEP registry. The action of building and publishing a new image
triggers the server to download that image and run it on the server.

### Build the Docker image

> See [Docker website](https://docs.docker.com/engine/install/) for installation instructions

```shell-script
docker build -t local-lockdown .
```

Due to the size of R dependencies, this will take ~15 minutes to build and take up around 2.2GB in space.

### Run the Docker container

Once you've built the image, you can use Docker to run it

```shell-script
docker run -it --rm -p 8080:3838 local-lockdown
```

Once that's running, you can visit [localhost:8080](http://localhost:8080) to
view the application.

### todos

- [ ] Add tests to ensure that the Docker image runs correctly
- [ ] Automate dependency installation using `requirements.txt` format (or similar)
- [ ] Establish better fix for error installing [tidyr@1.1.1](https://stackoverflow.com/questions/63348135/error-installing-tidyr-on-ubuntu-18-04-r-4-0-2)
- [ ] Run Actions on all branches, only publishing on `master`

## Contributing
To contribute to this project, please follow [GitHub Flow](https://guides.github.com/introduction/flow/) when submitting changes.

> Please note that this project is released with a Contributor Code of Conduct. By participating in this project you agree to abide by its terms.

## Getting help
If you encounter a clear bug, please file a minimal reproducible example in [issues](https://github.com/britishredcrosssociety/local-lockdown/issues).

