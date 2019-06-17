import i18next from "i18next";
import { initReactI18next } from "react-i18next";
import en from "./locales/en";

const locales = {
  en
};

const i18n = i18next.use(initReactI18next).init({
  lng: "en",
  resources: locales
});

export default i18n;
