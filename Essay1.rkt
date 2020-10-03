#lang scribble/manual

To: Denali project management
From: Andrew Lai
Date: 9/19/2020
Subj: Dev language for project Denali

On the previous business day the deveopment team for project Denali submitted a proposal 
to write our new mobile OS in Rust. Here is a summary of the largest influencers of the team's
proposal.

Firstly, users need consistency; we must select a programming language that is robust, that
will not crash easily, and that will perform for the entirety of the phone's lifespan. This rules
out many cutting edge languages and other languages that are still in the development phase today.
While this could be achieved with most languages, some are easier to write and debug in such a
way that makes this easy. Next, the language must be efficient; obviously, phones suffer from severe hardware and space
limitations and as such our source code must take full advantage of processing power. Another very important
influence in the decision of the source language is its popularity. If we select a popular language,
there is a higher chance that our developers will already have experience using it, reducing training
costs and saving time. Similarly, with a popular language, it will be more likely for third parties to
develop apps on our platform. As you know, we don't want to force third parties to learn some new, proprietary
language as a new competetor in the moble OS market. Popular languages also tend to have better documentation,
a larger support community, stabler source code, and better development tools. The last consideration I will bring up
for today is that we must select a language that our senior devs are comfortable with -- a language that
the team already has experience with. This is rather self-explanatory but it is critical that we have
employees who can lead development without having to worry about learning the language from scratch
themselves.

Rust would be our first choice that meets all of these conditions. Rust is a low level language that
works very closely to the hardware but is also easy to develop with. Like C, Rust can be optimized
extremely well to perform efficiently and consisently. And while it is a new guy on the programming
scene, it is already being used in a variety of applications and has a strong support base. One of our
technical leads was part of the team that developed the language so there is plenty of internal experience
for new hires to rely on. In the case Rust fails for some unforseen reason, we can always fall back to
C++ or C#, but our team believes it will be easier to develop more efficiently with Rust. All three will
be able to do the job, but the team feels that Rust has the ideal combiation of efficiency, low learning curve,
community support, and usability. Management proposed using Python or Java in preliminary design reviews 
however both languages will not be a good fit for what we are looking to achieve for the following reason: 
Both require a virtual environment to run; to run Python or Java code, there must be a Python or Java 
runtime engine behind it. This results in inefficient utilization of processing power and unnecessary 
usage of computer resources.

If there is no opposition brought up, we plan to begin sprint 1 next week and plan to have design review 2
in 2 months.