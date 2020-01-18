# Agora Frontend

This is Agora's frontend. This project helps to track proposals published in Tezos's blockchain.

## Run for development

Install all needed dependencies:

  * `$ npm install` or `$ yarn`

Run the development server:

  * Make sure the backend API is accessible at `localhost:8190`
  * `$ npm run serve:dev`
  * The server will be listening on `localhost:1234`

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

## Storybook

To create and view standalone versions of various components, you can use Storybook. See documentation [here](https://storybook.js.org/docs/basics/writing-stories/). However, be aware that to compile with Parcel.js, we're using the 5.3 beta version, so there may be some differences from these docs.

To run storybook:
`$ npm run storybook`

To create new stories, create a file named `[name].stories.tsx` in the `~/stories` subfolder. You probably want to use the `/stories/storyWrapper.tsx` Wrapper to provide base styles and internationalization.
