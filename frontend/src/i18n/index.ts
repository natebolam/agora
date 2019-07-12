import i18next from "i18next";
import { DateTime } from "luxon";
import { initReactI18next } from "react-i18next";
import humanizeDuration from "humanize-duration";
import en from "./locales/en";

const locales = {
  en,
};

const capitalizeFirstLetter = (value: string): string => {
  return value
    .split(" ")
    .map((word: string): string => {
      return word.charAt(0).toUpperCase() + word.slice(1);
    })
    .reduce((word: string, result: string): string => {
      return word + " " + result;
    });
};

const i18n = i18next.use(initReactI18next).init({
  lng: "en",
  resources: locales,
  interpolation: {
    // eslint-disable-next-line @typescript-eslint/no-explicit-any
    format: (value: any, format?: string, lng?: string): string => {
      if (format === "numberFormat") {
        return value.toLocaleString();
      }
      if (format === "dateFormat") {
        const date = DateTime.fromISO(value.date, { locale: lng });
        return date.toFormat(value.format, {});
      }
      if (format === "humanizeDateFormat") {
        return capitalizeFirstLetter(
          humanizeDuration(value.milliseconds, {
            language: lng,
            ...value.options,
          })
        );
      }
      if (format === "capitalizeFormat") {
        return capitalizeFirstLetter(value);
      }
      return "";
    },
  },
});

export default i18n;
