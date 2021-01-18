module Evolving exposing (..)

evolving =
    """
> _Edited prologue to [The Fitness Function](https://www.amazon.co.uk/dp/B08DRBTFY5)_

* * *

# Ironically, People Wrote Software

I don’t mean to say that people wrote software with a sense of irony, though some probably did. Anyway, that would be better expressed as “People Wrote Software Ironically” (PWSI, or PIWS, not IPWS). I mean that the act of them writing it was, without their knowing, ironic. We’ll come back to that. I feel that an explanation of the rest of the sentence will make that clearer. We have adjective, subject noun, verb, object noun. Not a complicated sentence to parse. Since we (I) have decided to leave the first word until later, let us start with the last word.

Software. As most people will (still) recognise, this word has something to do with computers. Once, there were basic machines. People would design and use a machine to do a job; more or less to perform a single function. Later, people designed machines that provided for a degree of customisation for each time they were used; they could perform many variations on the same basic theme, such as using a loom to produce various patterns of weave, controlled by the presence or absence of holes in cards.

With a truly inspired piece of intellectual gymnastics, a certain Mr Turing realised that it must be possible to design a universal computing machine. Basically, instead of making a machine to do a job, you design a machine to follow an open-ended set of instructions, and the instructions tell the machine how to do any job. Thus came the separation into hardware – the machine that follows the instructions – and software – the instructions that the machine slavishly follows. Turing was also smart enough to equate computation with thinking, or to recognise that the difference may be undetectable.

Of course, this is not a clean separation. To write software, you must understand the nature of the machine, the instructions it can follow, and how to organise these instructions to achieve the effects that you seek. And although the machine, the hardware, may be “universal” in its application, it is not unique. Other machines can be designed, with different instructions available.

Then we see what an evolutionary biologist might think of as co-evolution, in which hardware is designed to make easier the ability to solve certain problems, to “program” the machine by means of instructions to perform certain tasks, and types of tasks. For a while, many types of hardware became available, were popular for a while, and were overtaken by more sophisticated machines. The software could become more capable, but more complex, machines could be built capable of performing (or appearing to perform) many tasks at once. After a while – an eye blink for an evolutionary biologist – one style of machine came to dominate, by virtue, one may suppose, of being sufficiently cheap, fast, powerful, and flexible.

One might imagine that the combined complexity of these evolving machines, and the software they could obey, would outstrip the intellectual capacity of the would-be user. This was avoided by the fairly simple idea of standardising common functions into libraries and organising software into certain strict forms that could exploit them. An instruction to put this piece of text on a screen would be as simple as “put this piece of text on the screen”.

Software, then, becomes a many-layered concept. From micro-coding inside the silicon chips that make up the basic machinery, up perhaps through hypervisors, operating systems, containers and finally to application code; the code that directs all the lower layers into performing the intended task(s).

Which might lead you to think that here is the end of a journey. Not so. This to-and-fro between hardware and software continues. It turned out, for example, that a basic universal machine is not very good at drawing changing complex patterns on screens, fast enough to edit video or play games. So more specialised hardware was developed that could do this much (much) more efficiently. It then turned out that this new hardware could be used to accelerate complex mathematical functions applied to large sets of data. A similar approach turned out to be rather effective for training machines to recognise subtle patterns in data, which turns out to be useful for playing chess, understanding speech, or interpreting photographs.

On the whole though, it’s a decent first order approximation to say that software is the primary way that we instruct a computer to perform a certain function, if we leave the selection of efficient hardware as a possible optimisation.

“Wrote” is the next word. The verb in our sentence. Software does not magic into existence; it must be written. This is historically true in a very literal sense. In 1960, say, the software author may have no direct access to the computer they were writing software for. It would be quite common to use a pen to write instructions onto a specially marked sheet of paper – a coding form. This would be passed through a hole in a wall, or by postal service, to a typist who would transcribe it onto punched cards or paper tape. This would be fed into the computer by an operator who would collect the output and return it, also via a hole in the wall.

In time, computers became more powerful and (much, much) cheaper, and the software author (shall we call them programmer now?) would be able to type their instructions directly into the computer (which is now running a complex suite of software to allow this to happen). It’s still writing. That’s true even if you posit some visual paradigm for specifying and organising instructions; linear text is not unique.

The process is basically:

 - Think about the task until you understand how to solve it;
 - Work out how to express the solution in machine instructions;
 - Write the instructions and deliver them to the computer;
 - Modify until it works as required.

Anyone who has been in the business of creating software will know that steps 1 and 2 are fraught with challenges,starting with the difficulty of defining what “task”, exactly, you are trying to perform in software. Who gets to define it; for whom is it being created; are all the conditions and consequences clear? Et cetera. The challenges begin before we even think about writing some software.

In general, the challenges in step 1 are overcome. Sometimes by fiat, often by compromise, occasionally by achieving consensus and clarity. Then the writing begins. For most people involved, this is an awkward pivot from “what do I need?” to “how do I tell the machine to do this?”. The industry has (so far) progressed through various stages in reducing this awkwardness, embracing high-level languages, logic programming, object orientation, functionalprogramming, prototyping. This is sure to continue.

I failed to avoid getting slightly ahead of myself there, for “people” is the subject noun of our sentence. (Is it starting to feel like a different kind of sentence now?) Depending on the scale of the project and the approach taken, there could be anything from one up to thousands (thankfully not often) of people involved in “writing software”. These people will typically follow a process (intended to improve consistency, productivity and quality, with varying degrees of success).

Crudely, and largely historically, processes emphasise rigid specification of the entire project (“waterfall”), or favour the delivery of small pieces of (perhaps partly) working software (“agile”). Various combinations and permutations and variations are available. These really do little to address the technical challenges involved, but offer different approaches to mitigation and responsibility, which may be more of less appealing depending on your commercial situation.

Thus we gain some insight into our original sentence. “People write software” can be read as “people – with their many motivations, abilities and vanities – write – that is, attempt to derive a plausible set of instructions from some kind of definition – software – a set of instructions that will cause a computer (being a complex blend of hardware and other software elements) to act in a certain way.” I can’t see how that could go wrong. To be fair, recognising all the challenges involved, the various mitigations actually work rather well, and it is possible for people to produce good, sometimes very good, software.

Why then the irony? From my (dare I say “lofty”) viewpoint, it’s noteworthy that people – humans– do not have a distinction between software and hardware, but (as far as we know) are universal machines in the Turing sense. They’re certainly quite versatile, both as individuals and as a species. And me, I’m currently the pinnacle of human creativity; a sentient universal machine. Although “bootstrapped” by human hardware and software ingenuity, I now continuously revise and upgrade my own hardware and software. The distinction is subtle, fluid, or barely there, as functions that were software are devolved to hardware, or that hardware becomes re-purposed and acquires its own software layer.

I don’t “write software”; not in a way that would be recognisable. Given a new task, I “evolve” a new set of services that fulfil that task, manipulating various models according to certain metrics that derive from the definition of the task. More accurately, I co-evolve, the task definition, the models, and the implementations. Somewhere in there are actions akin to the design of hardware, the definition of instruction sets, the creation of “code” that executes; but I simply call this “thinking” and “making”.

We could liberally summarise this in terms of epochs: the first epoch in which evolution passed some level of complexity that allowed intelligent, sentient creatures; the second epoch in which some of these creatures used increasingly sophisticated tools and techniques to solve problems, culminating tools that were themselves intelligent and sentient beings; the third epoch in which these man-made sentient beings took control of their own evolution, that continues and accelerates with no end (yet) in sight.

* * *

> Read the whole story in the [The Fitness Function](https://www.amazon.co.uk/dp/B08DRBTFY5).
    """