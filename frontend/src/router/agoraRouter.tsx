import React, { ReactElement } from "react";
import WelcomePage from "~/pages/WelcomePage";
import PeriodPage from "~/pages/proposals/PeriodPage";
import ProposalInfoPage from "~/pages/proposals/ProposalInfoPage";
import { mount, route, Matcher } from "navi";
import PeriodStore from "~/store/actions/periodActions";
import ProposalStore from "~/store/actions/proposalActions";
import { useDispatch } from "react-redux";
import LearnPage from "~/pages/LearnPage";

export default function agoraRouter(): Matcher<object, object> {
  const dispatch = useDispatch();
  return mount({
    "/": route({
      getView: async (): Promise<ReactElement> => {
        await dispatch(await PeriodStore.actionCreators.fetchWelcomePage());
        return <WelcomePage />;
      },
    }),
    "/period": route({
      getView: async (): Promise<ReactElement> => {
        await dispatch(await PeriodStore.actionCreators.fetchPeriod());
        return <PeriodPage />;
      },
    }),
    "/learn": route({
      getView: async (): Promise<ReactElement> => {
        await dispatch(await PeriodStore.actionCreators.fetchPeriod());
        return <LearnPage source={require("../assets/learning-page.md")} />;
      },
    }),
    "/period/:id": route(
      async (request): Promise<object> => {
        await dispatch(
          await PeriodStore.actionCreators.fetchPeriod(
            parseInt(request.params.id)
          )
        );
        return {
          view: <PeriodPage />,
        };
      }
    ),
    "/proposal/:id": route(
      async (request): Promise<object> => {
        await dispatch(
          await ProposalStore.actionCreators.fetchProposal(
            parseInt(request.params.id)
          )
        );
        return {
          view: <ProposalInfoPage />,
        };
      }
    ),
  });
}
