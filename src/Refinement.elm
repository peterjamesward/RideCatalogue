module Refinement exposing (..)

import Element exposing (Element, centerX, clipY, fill, height, html, padding, paragraph, row, scrollbarY, width)
import Element.Background as Background
import Element.Font as Font
import FlatColors.FlatUIPalette exposing (silver, wetAsphalt)
import Markdown exposing (Options)
import Msg exposing (Msg)

viewMarkdown : String -> Element Msg
viewMarkdown str =
    row
        [ centerX
        , Background.color wetAsphalt
        , clipY
        , scrollbarY
        , height fill
        , padding 20
        ]
        [ paragraph
            [ width fill
            , height fill
            , Font.color silver
            ]
          <|
            [ html <| Markdown.toHtmlWith myOptions [] str ]
        ]

myOptions : Options
myOptions =
    { githubFlavored = Just { tables = True, breaks = False }
    , defaultHighlighting = Just "elm"
    , sanitize = True
    , smartypants = True
    }

stepwiseRefinement =
    """
 # What is Stepwise Refinement?

In 1971, Niklaus Wirth wrote a classic paper, [Program Development by Stepwise Refinement](http://sunnyday.mit.edu/16.355/wirth-refinement.html). The last of four conclusions he reached is

**Four**
> The detailed elaborations on the development of even a short program form a long story, indicating that careful programming is not a trivial subject. If this paper has helped to dispel the widespread belief that programming is easy as long as the programming language is powerful enough and the available computer is fast enough, then it has achieved one of its purposes.

Here at Stepwise Refinement, we feel that this applies to the design of any complex system. A good design should exhibit overall integrity and clarity of purpose. The integrity of the final solution arises because of a traceable series of elaborations that deal with specific challenges, perhaps around implementation choices or interaction with the environment. It is not just the result of these deliberations that needs to be conveyed, but the _story of their making_.

Do not think that Stepwise Refinement implies a simple methodical expansion of each box and line on an architecture view, fractally repeated until we turn it into code for cloud services and business logic. Not at all. It's a complex and long-winded to-ing and fro-ing, the application of various Patterns, and continual re-factoring. Yet running through all this is a _narrative_ that takes us from Purpose to realisation. That's exciting, and pleasing, and exactly what we think Niklaus was saying.

Despite having been written nearly **fifty years** ago, it's worth having a closer look at each of his conclusions. Firstly:

**One**
> Program construction consists of a sequence of refinement steps. In each step a given task is broken up into a number of subtasks. Each refinement in the description of a task may be accompanied by a refinement of the description of the data which constitute the means of communication between the subtasks. Refinement of the description of program and data structures should proceed in parallel.

Merely making subsitutions _program_ → _system_ and _task_ → _component_ yields:

**One (modified)**
> _System_ construction consists of a sequence of refinement steps. In each step a given _component_ is broken up into a number of sub-_components_. Each refinement in the description of a component may be accompanied by a refinement of the description of the data which constitute the means of communication between the sub-_components_. Refinement of the description of _components_ and data structures should proceed in parallel.

This is a useful succinct view of how architects and designers manage the approach to a complex system in a complex environment. Wirth is not constraining the nature of "task", so it could be an OS process, a service, a Hadoop cluster, whatever is needed. Structuring of data is given equal importance---the _lines_ on any system diagram are as important as the _boxes_.

Let us peruse the remaining conclusions, whilst mentally making the same substitions because we are system architects not programmers.

**Two**
> The degree of modularity obtained in this way will determine the ease or difficulty with which a program can be adapted to changes or extensions of the purpose or changes in the environment (language, computer) in which it is executed.

In modern parlance, we seek cohesion within modules and loose-coupling between them. We seek to avoid technology lock-ins. We anticipate and embrace change.

**Three (1)**
> During the process of stepwise refinement, a notation which is natural to the problem in hand should be used as long as possible. The direction in which the notation develops during the process of refinement is determined by the language in which the program must ultimately be specified, i.e. with which the notation ultimately becomes identical. This language should therefore allow us to express as naturally and clearly as possible the structure of program and data which emerge during the design process.

Awesome -- Domain Specific Languages! Don't rush to a solution without a full understanding of the problem; work in terms of the problem. Construct a mapping between the language most suitable for expressing the problem, and the languages used for its implementation. These days, we're perhaps talking about a notation from which, say, CloudFormation or Terraform scripts can be derived.

**Three (2)**
> At the same time, it must give guidance in the refinement process by exhibiting those basic features and structuring principles which are natural to the machine by which programs are supposed to be executed.

"It" being the DSL. With our substitutions, _machine_ refers to an execution environment that is probably much more than just a machine; _programs_ of course refers to our collection of components that execute tasks, in some complex but coordinated scheme. We should enhance our DSL so that we can reflect our preferred implenentation. This may be fundamental grammar, suitable libraries, or some means of annotation --- to indicate component boundaries or concurrent execution perhaps.

**Three (3)**
> It is remarkable that it would be difficult to find a language that would meet these important requirements to a lesser degree that the one language still used most widely in teaching programming: Fortran.

Fortran? Suddenly, we are reminded that this is the early 1970's. But, we must ask, is JavaScript suited to "the problem at hand"? Is Cloud formation, JSON or YAML? Probably not --- Wirth's cutting comment remains true.

**Three (4)**
> Each refinement implies a number of design decisions based upon a set of design criteria. Among these criteria are efficiency, storage economy, clarity, and regularity of structure.

Bring on the Non-Functional Requirements. 

**Three (5)**
> Students must be taught to be conscious of the involved decisions and to critically examine and to reject solutions, sometimes even if they are correct as far as the result is concerned; they must learn to weigh the various aspects of design alternatives in the light of these criteria.

Enter Architectural Trade-off Analysis. This is what separates the good from the average and the average from the bad. Decent decisions here will result in systems that **w**ork --- they will meet the functional requirements and probably be OK most of the time. Good decisions here will result in systems that **W**ork --- they will meet NFRs and they will continue to develop and improve over time.

**Three (6)**
> In particular, they must be taught to revoke earlier decisions, and to back up, if necessary even to the top. Relatively short sample problems will often suffice to illustrate this important point; it is not necessary to construct an operating system for this purpose.

In short, avoid technical debt and be willing to re-factor. Don't be happy with the first option, but always seek something better, even if you do not have the immeidate luxury of choosing it.

Wirth's fourth and final conclusion is the one we began with; that the narrative of refinement is (perhaps) as important as the outcome. It may not be obvious that this is so, if all you judge by is a working system. It is in the many years beyond, that having that narrative available will make it easier for anyone to understand the system and the forces that drove its design, and thereby to safely and confidently undertake changes to adapt the system to new requirements, new environments, and exploit new opportunities that were not available or made poor sense at the time. Stepwise Refinement; it never really ends.
"""
