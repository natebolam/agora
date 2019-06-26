import React, { FunctionComponent } from "react";
import cx from "classnames";
import Card from "~/components/common/Card";
import PieChart from "~/assets/png/pie_chart.png";
import ExternalIcon from "~/assets/png/external_icon.png";
import styles from "~/styles/components/proposals/ProposalPieChart.scss";

interface ProposalPieChartGraphTypes {
  className: string;
}

const ProposalPieChartGraph: FunctionComponent<ProposalPieChartGraphTypes>
  = ({className}) => {
  return (
    <div className={cx(className, styles.pieChart__pie__content)}>
      <img src={PieChart}/>
      <span>60</span>
    </div>
  );
};

const ProposalVotePieBody: FunctionComponent = () => {
  return (
    <div className={styles.pieChart}>
      <div className={styles.pieChart__wrapper}>
        <ProposalPieChartGraph className={styles.pieChart__pie}/>
        <ul className={styles.pieChart__legend}>
          <li className={styles.green}>Brasilia 2</li>
          <li className={styles.orange}>Brasilia 3</li>
          <li className={styles.blue}>Brasilia 1</li>
        </ul>
      </div>
      <a className={styles.pieChart__discusButton} href="#">Disqus on discource <img src={ExternalIcon}/></a>
    </div>
  );
};

interface ProposalPieChartTypes {
  className?: string;
  votes?: {
    caption: string;
    votes: number;
  };
}

const ProposalPieChart: FunctionComponent<ProposalPieChartTypes>
  = ({className}) => {

  return (
    <Card className={className} header={"Proposals"} body={<ProposalVotePieBody/>}/>
  );
};

export default ProposalPieChart;