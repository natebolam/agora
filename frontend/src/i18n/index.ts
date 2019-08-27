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
      if (format === "humanizeDurationFormat") {
        const date = DateTime.fromISO(value.date, { locale: lng });
        const millisecondsDiff = Math.max(
          date.diffNow("milliseconds").milliseconds,
          0
        );
        return capitalizeFirstLetter(
          humanizeDuration(millisecondsDiff, {
            language: lng,
            units: ["y", "mo", "d", "h", "m", "s"],
            round: true,
            ...value.options,
          })
        );
      }
      if (format === "humanizeDateFormat") {
        const date = DateTime.fromISO(value.date, { locale: lng });
        const startDate = date.startOf("day");
        const today = DateTime.local().startOf("day");

        if (today.diff(startDate, "days").days == 0) {
          return `Today at ${date.toFormat(value.timeFormat, {})}`;
        }
        if (today.diff(startDate, "days").days == 1) {
          return `Yesterday at ${date.toFormat(value.timeFormat, {})}`;
        }
        return date.toFormat(value.format, {});
      }
      if (format === "capitalizeFormat") {
        return capitalizeFirstLetter(value);
      }
      return "";
    },
  },
});

export default i18n;
