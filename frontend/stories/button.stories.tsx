import * as React from "react"
import { storiesOf } from "@storybook/react"
import {action} from "@storybook/addon-actions"
import styles from "../src/styles/pages/WelcomePage.scss";

import {ButtonLink} from "../src/components/common/ButtonLink"

const stories = storiesOf("Buttons", module)

stories.add("Button Link", () => (<>
    <ButtonLink href="https://www.google.com" >A Link Button</ButtonLink>
</>
))

stories.add("std button", () => (<button onClick={()=>{console.log('eh?'); action('engage-click')}}>A button</button>))
