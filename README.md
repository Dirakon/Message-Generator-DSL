<!-- Improved compatibility of back to top link: See: https://github.com/othneildrew/Best-README-Template/pull/73 -->
<a name="readme-top"></a>
<!--
*** Thanks for checking out the Best-README-Template. If you have a suggestion
*** that would make this better, please fork the repo and create a pull request
*** or simply open an issue with the tag "enhancement".
*** Don't forget to give the project a star!
*** Thanks again! Now go create something AMAZING! :D
-->



<!-- PROJECT SHIELDS -->
<!--
*** I'm using markdown "reference style" links for readability.
*** Reference links are enclosed in brackets [ ] instead of parentheses ( ).
*** See the bottom of this document for the declaration of the reference variables
*** for contributors-url, forks-url, etc. This is an optional, concise syntax you may use.
*** https://www.markdownguide.org/basic-syntax/#reference-style-links
-->
[![Contributors][contributors-shield]][contributors-url]
[![Forks][forks-shield]][forks-url]
[![Stargazers][stars-shield]][stars-url]
[![Issues][issues-shield]][issues-url]
[![MIT License][license-shield]][license-url]



<!-- PROJECT LOGO -->
<br />
<div align="center">

<h3 align="center">Message Generator DSL</h3>

  <p align="center">
    Domain-specific language that generates messages from source code with logic-programming-like syntax, variables, functions and conditions.
    <br />
    <a href="https://github.com/Dirakon/Message-Generator-DSL"><strong>Explore the docs »</strong></a>
    <br />
    <br />
    <a href="https://github.com/Dirakon/Telegram-Message-Generator-Bot">View example</a>
    ·
    <a href="https://github.com/Dirakon/Message-Generator-DSL/issues">Report Bug</a>
    ·
    <a href="https://github.com/Dirakon/Message-Generator-DSL/issues">Request Feature</a>
  </p>
</div>



<!-- TABLE OF CONTENTS -->
<details>
  <summary>Table of Contents</summary>
  <ol>
    <li>
      <a href="#about-the-project">About The Project</a>
    </li>
    <li>
      <a href="#getting-started">Getting Started</a>
      <ul>
        <li><a href="#prerequisites">Prerequisites</a></li>
        <li><a href="#installation">Installation</a></li>
      </ul>
    </li>
    <li><a href="#usage">Usage</a></li>
      <ul>
        <li><a href="#module-usage">Module Usage</a></li>
        <li><a href="#language-usage">Language Usage</a></li>
      </ul>
    <li><a href="#roadmap">Roadmap</a></li>
    <li><a href="#contributing">Contributing</a></li>
    <li><a href="#license">License</a></li>
  </ol>
</details>



<!-- ABOUT THE PROJECT -->
## About The Project
Domain-specific language that generates messages from source code with logic-programming-like syntax, variables, functions and conditions.



<p align="right">(<a href="#readme-top">back to top</a>)</p>



<!-- GETTING STARTED -->
## Getting Started

To get a local copy up and running follow these simple example steps.

### Prerequisites

You will need the following utilities installed globally:
* [npm](https://github.com/npm/cli)
* [purescript](https://www.purescript.org/)
* [spago](https://github.com/purescript/spago)

### Installation

1. Clone the repo
   ```sh
   git clone https://github.com/Dirakon/Message-Generator-DSL.git
   cd Message-Generator-DSl
   ```
2. Build project with globally installed spago
   ```sh
   spago build
   ```

<p align="right">(<a href="#readme-top">back to top</a>)</p>



<!-- USAGE EXAMPLES -->
## Usage

### Module usage
After installation, you can easily access the builded version in output folder, for example, from node in the following manner:
```
import { initializeVariables, generateOneMessage } from '../output/Main/index.js'

let metaInfo = initializeVariables(sourceCode);

let exampleMessage = generateOneMessage(metaInfo)();

```

Alternatively, you could use this module as a submodule in another spago project, like in [this example](https://github.com/Dirakon/Telegram-Message-Generator-Bot)

### Language usage

The language supports functions(macros), variables, comments, assertions; here's code example with all of them:

```
>>> This is a comment

>>> This is the main function definition, which is the entry point. In this example, it references some variable.
>>> (Variables start with '$', and functions start with '#')
#Main = $Variable

>>> This is variable definition.
>>> ('|' separates different possible values the expression can take, '+' concatenates two expressions)
$Variable = 
  "some actual string"
  | ($gender + " is " + $pronoun)

>>> This is multi-variable assignment. Think of it as destructuring assignment.
[$gender,$pronoun] = ["man","he"] | ["woman","she"]

>>> This is an assertion. It will filter out messages that were generated with variables not satisfying the condition.
$pronoun != "she"
```

The example code can generate the following messages:
```
[
  "some actual string",
  "man is he"
]
```


<p align="right">(<a href="#readme-top">back to top</a>)</p>



<!-- ROADMAP -->
## Roadmap

- [ ] Lift/add errors when applicable
- [ ] Document the functions
- [ ] Add fully functional pattern matching in assignments
- [ ] Fix message generation distribution with '|'
- [ ] Implement optimization for assertions depending on just one variable
- [ ] Add support for complex assertions (or/and)
- [ ] Add numeric values
    - [ ] Add functions related to them
- [ ] Add boolean values
    - [ ] Add functions related to them
- [ ] Add string-related functions

See the [open issues](https://github.com/Dirakon/Message-Generator-DSL/issues) for a full list of proposed features (and known issues).

<p align="right">(<a href="#readme-top">back to top</a>)</p>



<!-- CONTRIBUTING -->
## Contributing

1. Fork the Project
2. Create your Feature Branch (`git checkout -b feature/AmazingFeature`)
3. Commit your Changes (`git commit -m 'Add some AmazingFeature'`)
4. Push to the Branch (`git push origin feature/AmazingFeature`)
5. Open a Pull Request

<p align="right">(<a href="#readme-top">back to top</a>)</p>



<!-- LICENSE -->
## License

Distributed under the MIT License. See `license` for more information.

<p align="right">(<a href="#readme-top">back to top</a>)</p>




<!-- MARKDOWN LINKS & IMAGES -->
<!-- https://www.markdownguide.org/basic-syntax/#reference-style-links -->
[contributors-shield]: https://img.shields.io/github/contributors/Dirakon/Message-Generator-DSL.svg?style=for-the-badge
[contributors-url]: https://github.com/Dirakon/Message-Generator-DSL/graphs/contributors
[forks-shield]: https://img.shields.io/github/forks/Dirakon/Message-Generator-DSL.svg?style=for-the-badge
[forks-url]: https://github.com/Dirakon/Message-Generator-DSL/network/members
[stars-shield]: https://img.shields.io/github/stars/Dirakon/Message-Generator-DSL.svg?style=for-the-badge
[stars-url]: https://github.com/Dirakon/Message-Generator-DSL/stargazers
[issues-shield]: https://img.shields.io/github/issues/Dirakon/Message-Generator-DSL.svg?style=for-the-badge
[issues-url]: https://github.com/Dirakon/Message-Generator-DSL/issues
[license-shield]: https://img.shields.io/github/license/Dirakon/Message-Generator-DSL.svg?style=for-the-badge
[license-url]: https://github.com/Dirakon/Message-Generator-DSL/blob/master/license
