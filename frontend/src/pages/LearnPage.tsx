import React, {
  FunctionComponent,
  ReactElement,
  createRef,
  useEffect,
  RefObject,
} from "react";
import ReactMarkdown from "react-markdown";
import cx from "classnames";
import { Layout, LayoutContent } from "~/components/common/Layout";
import AgoraHeader from "~/components/common/Header";
import styles from "~/styles/pages/LearnPage.scss";
import Card from "~/components/common/Card";
import { useTranslation } from "react-i18next";

const LearnPage: FunctionComponent = (): ReactElement => {
  const { t } = useTranslation();
  const elRef: RefObject<HTMLDivElement> = createRef();
  const tocRef: RefObject<HTMLDivElement> = createRef();

  useEffect((): void => {
    const el = elRef.current;
    const toc = tocRef.current;
    if (!el || !toc) return;

    const headings = Array.from(
      el.querySelectorAll("h1, h2, h3, h4, h5, h6")
    ) as HTMLHeadingElement[];

    for (const heading of headings) {
      const tag = heading.tagName.toLowerCase();
      const link = document.createElement("a");
      link.href = `#${heading.id}`;
      link.className = cx(
        styles.learnPage__toc_link,
        styles[`learnPage__toc_link_${tag}`]
      );
      link.textContent = heading.textContent;

      toc.appendChild(link);
    }
  });

  return (
    <Layout>
      <LayoutContent className={styles.learnPage__header}>
        <AgoraHeader />
      </LayoutContent>
      <LayoutContent className={styles.learnPage__primaryInfo}>
        <h1 className={styles.learnPage__title}>{t("learnPage.title")}</h1>
        <div ref={tocRef}></div>
      </LayoutContent>
      <LayoutContent>
        <Card className={styles.learnPage__card}>
          <div ref={elRef}>
            <ReactMarkdown
              source={require("../assets/learning-page.md")}
              escapeHtml={false}
            />
          </div>
        </Card>
      </LayoutContent>
    </Layout>
  );
};

export default LearnPage;
