* language-plantuml

Haskell implementation of PlantUML's sequence diagram parser.

This is under evaluation for my own project and API is unstable at this moment.
I'd like to implement parser other than sequence diagram but as far as I know there is
no formal syntax description, implementing syntax requires much efforts to know how syntax should be 
organaized. So I'm confident that implementing more feature conflict to the current implemtation and
require overwhole of the implementation.

The work is based on the document described in the following URL.
https://plantuml.com/sequence-diagram

Features described in the above URL are mostly implemented except "Anchors and Duration" section.
And some of features have TODO.


* Notice for possible implementators

I guess PlantUML's syntax evolved based on feature request from users, 
that make syntax of the sequence diagram is a bit complicated. 
As far as I understand currently, the followings are significat reqson of its complication.

- Line oriented syntax adn block oriented syntax are mixed.
Notes or group.

- Variations
Arrow has may flavaor of syntax.




