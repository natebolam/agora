// eslint-disable-next-line @typescript-eslint/no-var-requires
const { defaults } = require("jest-config");

module.exports = {
  transform: {
    "^.+\\.tsx?$": "ts-jest",
  },
  moduleFileExtensions: [...defaults.moduleFileExtensions, "png"],
  moduleNameMapper: {
    "$~/(.+)^": "<rootDir>/src/$1",
    "\\.(jpg|jpeg|png|svg)": "<rootDir>/tests/utils/fileMock.ts",
    "\\.scss": "<rootDir>/tests/utils/styleMock.ts",
  },
  modulePaths: ["<rootDir>/src"],
};
