import React, { FunctionComponent, ReactElement } from "react";
import ReactMarkdown from "react-markdown";
import { Layout, LayoutContent } from "~/components/common/Layout";
import AgoraHeader from "~/components/common/Header";
import styles from "~/styles/pages/LearnPage.scss";
import Card from "~/components/common/Card";

const LearnPage: FunctionComponent = (): ReactElement => {
  return (
    <Layout>
      <LayoutContent className={styles.learnPage__header}>
        <AgoraHeader />
      </LayoutContent>
      <LayoutContent>
        <Card className={styles.learnPage__card}>
          <ReactMarkdown
            source={require("../assets/learning-page.md")}
            escapeHtml={false}
          />
        </Card>
      </LayoutContent>
    </Layout>
  );
};

export default LearnPage;
