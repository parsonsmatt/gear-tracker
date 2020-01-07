# gear-tracker

[![Build Status](https://travis-ci.org/parsonsmatt/gear-tracker.svg?branch=master)](https://travis-ci.org/parsonsmatt/gear-tracker)

It's good to track the mileage that you put on your gear.
This helps when knowing when it's time to do preventative maintenance, knowing how much distance/use you're getting out of your gear, and, when reselling items, how much use the next owner can expect.
This app was inspired by some of the shortcomings of Strava's feature to accomplish this task.

## Development:

The `Makefile` is documented and contains the common steps for building, testing, and documenting the project.
This project uses the `stack` build tool for development.

### Database

The program uses `postgresql` as a database.
It assumes that the database is named `geartracker` and that the current user is able to access the database.
Running `make init-db` will initialize the database and run the migrations.

### Code Style

- `stylish-haskell`

## Documentation:

The project is documented via `README.md` in the relevant directories and Haddock documentation.
For 'namespace' modules, like [`GT`](src/GT.hs), the module documentation serves as the 'directory' documentation.
