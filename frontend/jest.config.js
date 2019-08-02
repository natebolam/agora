// eslint-disable-next-line @typescript-eslint/no-var-requires
const { defaults } = require("jest-config");

module.exports = {
  transform: {
    "^.+\\.tsx?$": "ts-jest",
  },
  moduleFileExtensions: [...defaults.moduleFileExtensions, "png"],
  moduleNameMapper: {
    "\\.scss": "<rootDir>/tests/testUtils/styleMock.ts",
    "\\.(jpg|jpeg|png)$": "<rootDir>/tests/testUtils/fileMock.ts",
    "~/(.+)$": "<rootDir>/src/$1",
  },
  modulePaths: ["<rootDir>/src"],
};
