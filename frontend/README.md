# Agora Frontend

This is Agora's frontend. This project helps to track proposals published in Tezos's blockchain.

## How to run

Install all needed dependencies:
`$ npm install` or `$ yarn`
To start development server:
`$ npm run serve` or `$ yarn serve`

## Checks
To run all test:
`$ npm run check-all` or `$ yarn test`
To run unit test:
`$ npm run test` or `$ yarn test`
To compile typescript:
`$ npm run tscompile` or `$ yarn tslint`
To lint typescript:
`$ npm run tslint` or `$ yarn tslint`
To lint scss files:
`$ npm run stylelint` or `$ yarn stylelint`

## Hot to build container

To build frontend into the docker container you have to run nix-build from global project root
`$ nix-build docker.nix -A frontend-image`
