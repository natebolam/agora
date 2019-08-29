import React, { FunctionComponent, ReactElement } from "react";
import ReactMarkdown from "react-markdown";
import cx from "classnames";
import { Layout, LayoutContent } from "~/components/common/Layout";
import AgoraHeader from "~/components/common/Header";
import styles from "~/styles/pages/LearnPage.scss";
import Card from "~/components/common/Card";
import { useTranslation } from "react-i18next";
import BusyIndicator from "react-busy-indicator";
import { useLoadingRoute } from "react-navi";

interface ToCItemTypes {
  item: HTMLHeadingElement;
}

const TocItem: FunctionComponent<ToCItemTypes> = ({ item }): ReactElement => {
  return (
    <div
      className={cx(
        styles.learnPage__toc_link,
        styles[`learnPage__toc_link_${item.tagName.toLowerCase()}`]
      )}
    >
      <a href={`#${item.id}`}>{item.textContent}</a>
    </div>
  );
};

interface LearnPageProps {
  source: string;
}

const LearnPage: FunctionComponent<LearnPageProps> = ({
  source,
}): ReactElement => {
  const { t } = useTranslation();
  const loadingRoute = useLoadingRoute();

  const temp = document.createElement("div");
  temp.innerHTML = source;

  const headings = Array.from(
    temp.querySelectorAll("h1, h2, h3, h4, h5, h6")
  ) as HTMLHeadingElement[];

  return (
    <Layout>
      <BusyIndicator
        active={!!loadingRoute}
        delayMs={0}
        className={""}
        color={"blue"}
        isBusy={!!loadingRoute}
        style={{}}
      />
      <LayoutContent className={styles.learnPage__header}>
        <AgoraHeader />
      </LayoutContent>
      <LayoutContent className={styles.learnPage__primaryInfo}>
        <h1 className={styles.learnPage__title}>{t("learnPage.title")}</h1>
        <div>
          {headings.map(
            (heading: HTMLHeadingElement, index: number): ReactElement => (
              <TocItem item={heading} key={index} />
            )
          )}
        </div>
      </LayoutContent>
      <LayoutContent>
        <Card className={styles.learnPage__card}>
          <ReactMarkdown source={source} escapeHtml={false} />
        </Card>
      </LayoutContent>
    </Layout>
  );
};

export default LearnPage;
