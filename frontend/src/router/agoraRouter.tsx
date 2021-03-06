import React, { ReactElement } from "react";
import StagePage from "~/pages/proposals/StagePage";
import ProposalInfoPage from "~/pages/proposals/ProposalInfoPage";
import { mount, route, Matcher, redirect } from "navi";
import StageStore from "~/store/actions/stageActions";
import ProposalStore from "~/store/actions/proposalActions";
import { useDispatch } from "react-redux";
import WelcomePage from "~/pages/WelcomePage";

export default function agoraRouter(): Matcher<object, object> {
  const dispatch = useDispatch();
  return mount({
    "/embed": route({
      getView: async (): Promise<ReactElement> => {
        await dispatch(await StageStore.actionCreators.fetchWelcomePage());
        return <WelcomePage />;
      },
    }),
    "/stage": route({
      getView: async (): Promise<ReactElement> => {
        await dispatch(await StageStore.actionCreators.fetchStage());
        return <StagePage />;
      },
    }),
    "/stage/:id": route(
      async (request): Promise<object> => {
        await dispatch(
          await StageStore.actionCreators.fetchStage(
            parseInt(request.params.id)
          )
        );
        return {
          view: <StagePage />,
        };
      }
    ),
    "/proposal/:stage/:id": route(
      async (request): Promise<object> => {
        await dispatch(
          await ProposalStore.actionCreators.fetchProposal(
            parseInt(request.params.stage),
            parseInt(request.params.id)
          )
        );
        return {
          view: <ProposalInfoPage />,
        };
      }
    ),
    "/": redirect("./stage"),
  });
}
