## :white_check_mark: Checklist for your Pull Request

<!--
Ideally a PR has all of the checkmarks set.

If something in this list is irrelevant to your PR, you should still set this
checkmark indicating that you are sure it is dealt with (be that by irrelevance).

If you don't set a checkmark (e. g. don't add a test for new functionality),
you must be able to justify that.
-->

#### Related changes (conditional)

- [ ] Tests
  - If I added new functionality, I added tests covering it.
  - If I fixed a bug, I added a regression test to prevent the bug from
        silently reappearing again.

- [ ] Documentation
  - I checked whether I should update the docs and did so if necessary:
    - [README](/README.md);
    - Haddock;
    - [Examples](/examples/).

- [ ] Public contracts
  - Any modifications of public contracts comply with the [Evolution
  of Public Contracts](https://serokell.notion.site/Evolution-of-Public-Contracts-2a3bf7971abe4806a24f63c84e7076c5) policy.
  - I added an entry to the [changelog](../tree/master/CHANGES.md) if my changes are visible to the users
        and
  - provided a migration guide for breaking changes if possible.

#### Stylistic guide (mandatory)

- [ ] Style
  - My commits comply with [the policy used in Serokell](https://serokell.notion.site/Commit-and-PR-policy-4cf98e1a910a415d86b5f2491d9af1af).
  - My code complies with the [style guide](https://github.com/serokell/style/blob/master/haskell.md).
  - Each commit of this pull request contains a description.
    - For new features, this description contains the use case for the new functionality.
    - For bug fixes, it describes both the attacked problem and a solution for it.
    - For dependency version changes description shortly enlists features and breaking changes of the new library version or contains a link to its changelog.
