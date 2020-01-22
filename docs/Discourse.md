# Integration with Discourse

Agora is an on-chain governance explorer. When a new proposal appears,
only its hash, short description, and policy URLs are known.
To obtain additional details about the proposal and keep them up-to-date,
Agora communicates with [Discourse].

## Setup

To connect Agora to your Discourse installation, fill in the
`discourse` section of the configuration file:

* `host` is the URL of your Discourse installation
* `api_username` is the name of Agora’s user
* `api_key` is an API key (you can make one in the “API” section of
  Discourse settings)
* `category` is the name of the category used for proposals

Restrict who can create topics in this category to the Agora user and staff.

## Topic matching

When Agora discovers a new proposal submitted to the blockchain, it will
first try to find an existing topic in Discourse. It does so by looking
at topic titles, and checking if any of them contains a prefix of the
proposal’s hash enclosed in either `()` or `[]`. If such a topic is found,
then it’s a match.

Example of a topic name: `Hello World (54b81a084)`.

If no matching topic was found, Agora will proceed to create a new topic
for this proposal, which you can then edit to add details of the new proposal.

## Updating proposal details

Topics corresponding to known proposals are periodically checked for updates
and proposal details are extracted from them and synchronised to Agora.

Only the first post of each topic is processed, so it is safe for users to
discuss the proposal in the topic. The format of the first post is the following:

```
<p>Short description.</p>
Long description
```

that is, the first paragraph will become the short description of the
proposal (it is best to keep it no longer than a couple of words) and the
rest will become the long description.

Note that the short description from Discourse will override the one from
the contract storage.


[Discourse]: https://discourse.org/
