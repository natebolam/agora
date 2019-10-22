# An introduction to Tezos Governance

Tezos is a self-amending blockchain software which uses an on-chain process to propose, select, test, and activate protocol upgrades [without the need to hard fork](https://medium.com/tezos/there-is-no-need-for-hard-forks-86b68165e67d). In practice, this enables Tezos to improve itself over time via a structured, yet decentralized process while preserving a high level of consensus.

Tezos also allows stakeholders to upgrade the amendment process itself. As a result, details of the mechanism described below represent the current mechanism and are subject to change. This page will evolve as the network evolves.

## Voters / Bakers

Baking is how blocks are produced and validated on a Tezos blockchain using Liquid Proof-of-Stake. Bakers (also known as "delegates") obtain the right to create (i.e. bake) a block when a roll of tokens (1 roll = 8,000 ꜩ) they own (or that is delegated to them) is randomly selected to produce or validate a block.

As the maintainers of a Tezos network, **bakers are also the voters in a Tezos formal upgrade process**, with their votes proportional to the size of their stake (including delegations).

## Votes / Rolls

To speed up computations for deciding which delegates are selected to bake, the Tezos ledger tracks tokens for staking and governance purposes as "rolls". Rolls are aggregated at the delegate level, which means a baker’s baking power is proportional to the amount of tokens delegated to them, rounded down to the nearest roll. A roll is currently set to 8,000 ꜩ.

## Delegators

If someone does not have 8,000 ꜩ or does not want to set up computing infrastructure to bake blocks, they may delegate their tokens to a baker. The baker does not own or control the delegated tokens in any way. In particular, it cannot spend them. However, if and when one of these tokens is randomly selected to bake a block, that right will belong to the baker. In practice, bakers usually share the additional revenue generated from the delegated tokens with the coin holder.

## The Four Stages of Tezos Governance

The amendment process can be broken into four discrete periods: the Proposal Period, the Exploration Period, the Testing Period, and the Promotion Period. Each of these four periods lasts eight baking cycles (i.e. 32,768 blocks or roughly 22 days, 18 hours), comprising almost exactly three months from proposal to activation.

As summarized in the flowchart diagram below, any failure to proceed to the subsequent period reverts the network back to a Proposal Period. In other words, failure to proceed restarts the entire amendment process.

### Proposal Period

The Tezos amendment process begins with the Proposal Period, during which bakers can submit proposals on-chain using the *proposals* operation, which involves specifying one or multiple protocol hashes, each one representing a tarball of concatenated .ml/.mli source files.

Bakers may submit up to 20 proposals in each Proposal Period. When submitting a proposal, the baker is also submitting a vote for that proposal, equivalent to the number of rolls in its staking balance at the start of the period.

For those wanting to follow along, Tezos Agora and other Tezos block explorers such as [TzStats](https://tzstats.com/) allow you to watch incoming proposals.

Other bakers can then vote on proposals by submitting *proposals* operations of their own. As described in the [whitepaper](https://tezos.com/static/white_paper-2dc8c02267a8fb86bd67a108199441bf.pdf), the Proposal Period vote is done via approval voting, meaning each baker may vote once on up to 20 proposals. Think of it as a form of “upvoting.”

At the end of the Proposal Period, the network counts the proposal votes and the most-upvoted proposal proceeds to the Exploration Period. If no proposals have been submitted or if there is a tie between proposals, a new Proposal Period begins.

### Exploration Period

In the Exploration Period, bakers may vote on the top-ranked proposal from the previous Proposal Period using the *ballot* operation. Bakers get to vote either "Yay", "Nay", or "Pass" on a specific proposal. "Pass" just means to abstain from voting for or against a proposal. As in the Proposal Period, a baker's vote is based on the number of rolls in its staking balance at the start of the period.

At the end of the Exploration Period, the network counts the votes. If voting participation meets the quorum, and an 80% supermajority of non-abstaining bakers approves, the proposal proceeds to the Testing Period.

If the voting participation fails to achieve the quorum or the 80% supermajority is not met, the amendment process restarts to the beginning of the Proposal Period.

Regardless of the outcome of the vote, the quorum is updated based on past participation rates.

### Testing Period

If the proposal is approved in the Exploration Period, the Testing Period begins with a testnet fork that runs in parallel to the main network for 48 hours.

This Testing Period is used to determine whether a proposal is a worthy amendment to the protocol. The testnet fork ensures the upgrade does not corrupt the blockchain network; should the upgrade be adopted, the network would continue making valid state transitions.

### Promotion Period

At the end of the Testing Period, the Promotion Period begins. In this period, the network decides whether to adopt the amendment based on off-chain discussions and its behavior during the Testing Period. As in the Exploration Period, bakers submit their votes using the *ballot* operation, with their votes weighted proportionally to the number of rolls in their staking balance.

At the end of the Promotion Period, the network counts the number of votes. If the participation rate reaches the quorum and an 80% supermajority of non-abstaining bakers votes “Yay,” then the proposal is activated as the new mainnet.

Regardless of the outcome of the vote, the process reverts back to the Proposal Period and the quorum is updated based on past participation rates.

## The Supermajority and Quorum Requirements

A vote during a voting period (Exploration & Promotion) needs to reach both a supermajority and a quorum (minimum participation rate) in order to succeed.

**Supermajority requirement:** The number of "Yay" votes divided by the number of "Yay" and "Nay" votes must be greater than or equal to 80%.

**Quorum requirement:** The number of "Yay", "Nay", and "Pass" votes divided by the number of possible votes must be greater than or equal to the current quorum.

Unlike the supermajority requirement which is fixed at 80%, the quorum requirement is updated at the end of each voting period using the following formula, where Q is the quorum in the voting period and q is the participation rate in the voting period:

![](png/quorum_update_formula.png)

In other words, the quorum tries to match the exponential moving average of the past participation rate.

## Flowchart of the Tezos Amendment Process

![](png/Tezos_governance_mechanism.png)

<h1 id="commands">Tezos Client Commands</h1>

<h2 id="proposals">Voting During a Proposal Period</h2>

```
$ tezos-client submit proposals for <delegate> <proposal1> <proposal2> ...
```

<h2 id="ballot">Voting During an Exploration or Promotion Period</h2>

```
$ tezos-client submit ballot for <delegate> <proposal> <yay|nay|pass>
```

<h2 id="status">Checking the Status of a Voting Period</h2>

```
$ tezos-client show voting period
```

# Additional Resources

- [The Voting Process](https://tezos.gitlab.io/mainnet/whitedoc/voting.html) from Nomadic Labs

- [Amending Tezos](https://medium.com/tezos/amending-tezos-b77949d97e1e) from Jacob Arluck
  - [Chinese translation](https://tezos.org.cn/amendingtezos/)
