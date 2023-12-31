# One Time Password Authentication Basics

[Cette page en français.](LISEZMOI.md)

Basis for supporting a password-free member area on a website.

Authentication will be based on an email address (as many (too many) websites do) or a telephone number (as TikTok does).

When accessing by e-mail, a one-time code is generated and sent by e-mail. Simply enter it on the screen to validate the e-mail and log in (or register).

In the case of access by telephone number, an OTP[^1] code will be sent by text message (via online APIs enabling SMS transmission). Connection (and registration) to the member area will be done by re-entering the number. Each code sent is valid for a few minutes and can only be used once.

**While password-less authentication by text message is relatively intimate (you need user's phone), password-less authentication by email is clearly a technique to be avoided, as it's the first thing hackers hack. Even if this repository is technically functional, I don't recommend using it when private, personal or sensitive data is available after authentication.**

[^1] OTP = one-time password

## Install

To download this project you better should use "git" command but you also can download a ZIP from [its GitHub repository](https://github.com/DeveloppeurPascal/OTP-Authentication-Basics).

**Warning :** if the project has submodules dependencies they wont be in the ZIP file. You'll have to download them manually.

A database SQL file contains the description of the "users" table used by PHP programs. You need to add it to your database and configure the program settings in the ./src/protected/config-dist.inc.php file by copying it as explained in it.

## How to ask a new feature, report a bug or a security issue ?

If you want an answer from the project owner the best way to ask for a new feature or report a bug is to go to [the GitHub repository](https://github.com/DeveloppeurPascal/OTP-Authentication-Basics) and [open a new issue](https://github.com/DeveloppeurPascal/OTP-Authentication-Basics/issues).

If you found a security issue please don't report it publicly before a patch is available. Explain the case by [sending a private message to the author](https://developpeur-pascal.fr/nous-contacter.php).

You also can fork the repository and contribute by submitting pull requests if you want to help. Please read the [CONTRIBUTING.md](CONTRIBUTING.md) file.

## Dual licensing model

This project is distributed under [AGPL 3.0 or later](https://choosealicense.com/licenses/agpl-3.0/) license.

If you want to use it or a part of it in your projects but don't want to share the sources or don't want to distribute your project under the same license you can buy the right to use it under the [Apache License 2.0](https://choosealicense.com/licenses/apache-2.0/) or a dedicated license ([contact the author](https://developpeur-pascal.fr/nous-contacter.php) to explain your needs).

## Support the project and its author

If you think this project is useful and want to support it, please make a donation to [its author](https://github.com/DeveloppeurPascal). It will help to maintain the code and binaries.

You can use one of those services :

* [GitHub Sponsors](https://github.com/sponsors/DeveloppeurPascal)
* [Liberapay](https://liberapay.com/PatrickPremartin)
* [Patreon](https://www.patreon.com/patrickpremartin)
* [Paypal](https://www.paypal.com/paypalme/patrickpremartin)

or if you speack french you can [subscribe to Zone Abo](https://zone-abo.fr/nos-abonnements.php) on a monthly or yearly basis and get a lot of resources as videos and articles.
