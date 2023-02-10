
<script
  src="https://cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-AMS-MML_HTMLorMML"
  type="text/javascript">
</script>


# TidyverseSkeptic
An opinionated view of the Tidyverse "dialect" of the R language, and
its promotion by RStudio.

## Norm Matloff, Prof. of Computer Science, UC Davis (former Prof. of Statistics at UCD)


Note:  This essay is somewhat frank, involving the very
popular Tidyverse and RStudio. I hope it is polite and taken as
constructive criticism.  

I like and admire the RStudio people, including the Tidyverse
originator, Hadley Wickham, and have always supported them, both
privately and
[publicly](https://matloff.wordpress.com/2018/02/22/xie-yihui-r-superstar-and-mensch/).
I have been interacting with them from the beginning, when the firm
consisted of only founder JJ Allaire and ace developer Joe Cheng, and of
course knew Hadley before that. (He and I have served as internal
reviewers for each other's books.) I highly praise the firm to my
students, and I use and recommend Hadley's **ggplot2** package (though I
don't consider it part of the Tidyverse, having been developed well
before Tidy and thematically unrelated).

In other words, I absolutely don't consider RStudio to be some evil
cabal.  I state at various places in this essay that I think their 
actions have been well-intentioned.  Nevertheless, I believe that
RStudio took a wrong turn when it decided to promote the Tidyverse,
which has led to a situation in which the very health of the language is
at stake.

[My bio is here.](http://heather.cs.ucdavis.edu/matloff.html)
Specifically in terms of R, I've been an R user and developer since near the 
beginning, having used R's predecessor S before that.  I've 
published several books that use R, and am currently (2019) the 
Editor-in-Chief of the *R Journal*.  (Hadley is a former EiC on the
journal.)

# Summary

$x^2$

1. The Tidyverse arose from the desire to have a set of packages that
   are consistent with each other, a "purist" philosophy that appeals,
for instance, to theoretical computer scientists.  The Tidyverse also
borrows from other "purist" computer science philosophies, notably
*functional programming*.  

2.  Unfortunately, the price of purity is (a) **increased complexity and
    abstraction**, making code more prone to error, as well as (b) **a
sacrifice in performance**.  Ironically, though consistency of interface
is a goal, new versions of Tidy libraries are often incompatible with
older ones, a very serious problem in the software engineering world.

3. In heavily promoting the Tidyverse, especially in the education
   realm, RStudio, with its dominance in the R field, is developing an
entire new generation of R users whose skills in base-R are superficial
at best, and who -- most importantly -- feel that R *is* the Tidyverse.

4. Regardless of being well-intentioned, **RStudio is molding R into its
   own desired image**.  That new generation will come to dominate the
community, treating Tidyverse as the "real" base, and viewing the actual
base-R as something akin to assembly language.  This might be fine if
the R community were unified in viewing Tidyverse as a high-level
improvement, but many do not; they are not fans of the syntax and so on,
and as noted, are worried about the slow performance of Tidy.

5.  That new generation will often be biased against non-Tidyverse job
    seekers, non-Tidyverse CRAN packages, and academics who submit
non-Tidyverse data science research papers and grant proposals.  They
will have no choice but to bend to RStudio's wishes.  Then
**RStudio will have succeeded in an end-run around the governing
body of the R language**.

6. For the above reasons, **RStudio is essentially operating as a
   monopolist**.  It is not a financial monopoly, since R is free and
RStudio does not directly benefit monetarily from the Tidyverse, but it
is a classical monopoly in terms dominance of a single product, unfair
competition and stifled innovation.  Though again this is
well-intentioned and the severity of the dominance is not yet complete,
but adverse impacts are very serious and worsening.

7. **The popularity of the Tidyverse is thus not a case a superior product
   rising to the top in a free market**.  RStudio has seven or eight
programmers working full time on developing Tidy, and the firm engages
in broad, vigorous actions to promote the product, especially with the
educational community.  This is in great contrast to the open-source
model of a large number of developers working in their spare time.

8. One key example of the pernicious effects of this monopolistic
   situation is that RStudio's **promotion of the Tidyverse has alarmingly
impeded the progress and adoption of technologically superior packages**,
notably **data.table**. Most in the "Tidyverse generation" are unaware
of anything outside the Tidyverse.  For instance, Hadley's book, *R for
Data Science*, with Garrett Grolemund, barely mentions **data.table**.

9. RStudio's development of the Tidyverse is a good thing, and those who
   like its philosophy should use it.  My objection is in Point 5 above.
I would give as an example the fact that R has various object-oriented
programming (OOP) paradigms to choose from, such as S3, S4 and R6.  I
think it's great that, e.g., R6 is available, but I don't want to be
forced to use it. (Just an example; both the Tidyverse and I use S3.)

10. A major reason offered by RStudio for promoting the Tidyverse is that
   it makes R easier to teach to non-programmers.  As a longtime,
award-winning educator, I very strongly dispute this.  **I would argue that the
Tidyverse makes R *harder* to learn**.

11. **RStudio could easily remedy the situation**.  I have recommendations at
   the end of this essay.

# dplyr vs. data.table

The **dplyr** package is a featured app of the Tidyverse developed by
Hadley, so I'll use this as an example at several points in this essay.
Note, though, that it is merely an example, not the core point of this
essay.

**Dplyr** borrowed a number of ideas from the earlier **data.table** by
Matt Dowle.  One of Hadley's major motivations was to give the user a
more "English-like" interface.  (Note: I regard both **dplyr** and
**data.table** as advanced topics; neither is suitable for beginners.)

Unfortunately, **dplyr** is much, much slower than
**data.table** on large datasets.  Here are some of the
timing examples, for various operations, on the 
[H2O site](https://h2oai.github.io/db-benchmark/)
(times in seconds; see above URL for details):

<table border="1">
<tr>  <td>dplr</td>  <td>data.table</td>  </tr> 
<tr> <td>37.3</td>  <td>9.07</td> </tr>
<tr> <td>95.5</td>  <td>9.20</td> </tr>
<tr> <td>496</td>  <td>11.9</td> </tr>
</table>

The differences are even starker in
[this study](http://www.win-vector.com/blog/2019/05/timing-working-with-a-row-or-a-column-from-a-data-frame/)
by the consulting firm Win-Vec LLC, e.g.

![alt text](https://i0.wp.com/www.win-vector.com/blog/wp-content/uploads/2019/05/unnamed-chunk-1-2.png)

showing that **dplyr** can be extremely slow even relative to base-R,
thus even worse relative to **data.table**.

RStudio, a commercial entity, is heavily engaged in educational
activities, including interacting with teachers of R.  It has heavily
promoted **dplyr**.  Meanwhile, RStudio and its allies have typically
ignored **data.table** in these promotions, e.g.  not mentioning it in
their timing comparisons, and when they do mention the software, paint
it as beyond the reach of nonprogrammers.

Hadley does have an interface, **dtplyr**, but it is 
[slow](http://www.win-vector.com/blog/2019/06/data-table-is-much-better-than-you-have-been-told/).

RStudio is after all a for-profit business, so such treatment of
**data.table** is just good business practice.  But R is an open-source
language, and these actions have harmed R. Advocates of other
languages, notably Python, constantly denigrate R as being slow on large
datasets.  Actually, **data.table** is extremely fast (and is faster
than Python's Pandas), but those who view R through the RStudio lens are
unaware of it.

# Teachability

Teaching has been a keen interest of mine since my college days.  I've
been a teacher of stat and computers for many years, and have won
various teaching awards.  My textbook, *Statistical Regression and
Classification: from Linear Models to Machine Learning*, was the
recipient of the 2017 Ziegel Award.
 
But it goes far beyond that; I really am intensely interested in how
people learn, from children to middle-aged adults.  Among other things,
I've taught English As a Second Language to immigrant adults, most of
whom have had less than a high school education.

### The Tidyverse advocates' claim

From this background, I strongly dispute the claim made by Tidyverse
advocates that it facilitates the teaching of R to beginning
programmers, as opposed to teaching base-R.  (Again, I regard both
**dplyr** and **data.table** as advanced topics; neither is suitable for
beginners.)

There has been no study of this claim.  Advocates often provide
testimonials from students like "I learned R using Tidyverse, and now am
productive in R!" -- which says nothing at all about the teachability of
base-R in comparison.  (It is ironic that advocates who present such
statements are statisticians, who ought to know the need for a control
group.)

Moreover, in discussions with those who report success in using
the Tidyverse to teach beginning programmers, I ask them whether their
students are incapable of learning just base-R.  They readily concede
that the answer is no.  Indeed, before the Tidyverse, throngs of people
were learning base-R without any prior programming background.

### Tidyverse makes learning harder, due to adding much complexity

Contrary to the Tidy advocates' claim, I believe using the Tidyverse
makes things *more* difficult for learners without prior programming
background.  

Tidyverse students are being asked to learn a much larger volume of
material, which is clearly bad pedagogy.  See ["The Tidyverse
Curse"](https://www.r-bloggers.com/the-tidyverse-curse), in which the
author says *inter alia* that he uses "only" 60 Tidyverse functions --
60!  The "star" of the Tidyverse, **dplyr**, consists of 263 functions.
While a user initially need not use more than a small fraction of them,
the high complexity is clear.  Every time a user needs some variant of
an operation, she must sift through those hundreds of functions for one
suited to her current need.  

As Matt Dowle [pointed
out](https://twitter.com/MattDowle/status/1142001162230489088),

> It isn't one function mutate that you combine in a pipe.  Its mutate,
> mutate_, mutate_all, mutate_at, mutate_each, mutate_each_, mutate_if,
> transmute, transmute_, transmute_all, transmute_at and transmute_if.
> And you're telling me you don't need a manual to learn all those?

The Tidiers' reply was essentially the standard, "But there is
consistent syntax for all those functions," which is nice, but the
salient fact is that the user must learn the *semantics* of all those
functions.

By contrast, if she knows base-R (not difficult), she can handle any
situation.  The old adage applies: "Give a man a fish, and he can eat
for a day. Teach him how to fish, and he can eat for a lifetime."

### Tibbles

Similarly, it is bad pedagogy to force students to learn tibbles, a
more complex technology, instead of data frames, a simpler one.

### Use of functional programming 

Another featured Tidyverse package, the *functional programming*
(FP)-oriented library **purrr**, has 177 functions.  Again the point
about complexity applies. Even more importantly, top university Computer
Science (CS) Departments have shifted away from teaching their
introductory programming courses using functional programming paradigm
to the more traditional Python, in part because they deem FP to be more
abstract and challenging.

An interesting discussion of the topic is in [Charavarty and
Keller](https://www-ps.informatik.uni-kiel.de/~mh/reports/fdpe02/papers/paper15.ps.gz).
They too believe FP in its standard form in introductory programming
classes is unsuitable even for CS majors.  It would then seem that using
FP to teach non-programmers learning R is even more unwise.

Note that even if taught in a modified FP form, the authors' goals are
antithetical to those of R learners.  They list three goals, one of
which is to "Introduce the essential [theoretical] concepts of 
computing," certainly not desirable for teaching R in general, let alone
for teaching R to those with no coding experience.

### The English issue

Again, the claim is that the Tidyverse is more teachable because of its
"English-like" syntax, while they dismiss **data.table**'s syntax as
opaque.

Below is a comparison (adapted from
[here](https://atrebas.github.io/post/2019-03-03-datatable-dplyr/)):
We'll use R's built-in **mtcars** dataset.

``` r
mtdt <- as.data.table(mtcars)
mtdt[cyl == 6]  # data.table syntax
mttb <- as_tibble(mtcars)
filter(mttb,cyl == 6)  # dplyr syntax
```

Is there really any difference?  Can't beginners, even without
programming background, quickly adapt to either one after seeing a few
examples?  Even those who claim high teachability for **dplyr** do
readily agree that their students could also easily pick up
**data.table**, or for that matter my preference base-R, given some
examples.

And what of the fact that we have the English word *filter* above?
Granted, it looks nice, but English can be misleading or mystifying in a
computer context.  Even an experienced programmer would not be able to
guess what the **dplyr** function **mutate()** does, for instance.

Moreover, the Tidy advocates don't like even the English in base-R, the
"apply" functions.  Furthermore, it is very tellling that many have
never even heard of the most powerful one, **tapply()**.  For instance, in a
[Web
page](https://tavareshugo.github.io/data_carpentry_extras/base-r_tidyverse_equivalents/base-r_tidyverse_equivalents.html)
comparing Tidy to base-R, there is this example:

``` r
tidyverse

mtcars %>% 
  group_by(cyl, gear) %>% 
  summarise(mpg.mean = mean(mpg),
            mpg.sd = sd(mpg),
            wt.mean = mean(wt),
            wt.sd = sd(wt)) %>% 
  ungroup() # remove any groupings from downstream analysis

base R

# First operate in the data.frame by group (split-apply)
mtcars_by <- by(mtcars, 
   INDICES = list(mtcars$cyl, mtcars$gear),
   FUN = function(x){
     data.frame(cyl = unique(x$cyl),
                gear = unique(x$gear),
                mpg.mean = mean(x$mpg),
                mpg.sd = sd(x$mpg),
                wt.mean = mean(x$wt),
                wt.sd = sd(x$wt))
   })

# Then combine the results into a data.frame
do.call(rbind, mtcars_by)
```

The output of the Tidy version is

``` r
# A tibble: 8 x 6
    cyl  gear mpg.mean mpg.sd wt.mean  wt.sd
  <dbl> <dbl>    <dbl>  <dbl>   <dbl>  <dbl>
1     4     3     21.5 NA        2.46 NA    
2     4     4     26.9  4.81     2.38  0.601
3     4     5     28.2  3.11     1.83  0.443
4     6     3     19.8  2.33     3.34  0.173
5     6     4     19.8  1.55     3.09  0.413
6     6     5     19.7 NA        2.77 NA    
7     8     3     15.0  2.77     4.10  0.768
8     8     5     15.4  0.566    3.37  0.283
```

Yet the above lengthy R code can be done much more compactly. E.g 

``` r
with(mtcars,tapply(mpg,list(cyl,gear),mean))
```

with output 

``` r
      3      4    5
4 21.50 26.925 28.2
6 19.75 19.750 19.7
8 15.05     NA 15.4
```

Not only is the code highly compact, but also the output is nicer, in
table form.  (If desired in data frame form, the output can be run
through **as.data.frame(as.table())**.)

### Back to reality

It is absolutely untenable to argue that

``` r
mtcars %>% 
  group_by(cyl, gear) %>% 
  summarise(mpg.mean = mean(mpg)) %>% 
  ungroup() 
```

is easier to learn than

``` r
with(mtcars,tapply(mpg,list(cyl,gear),mean))
```

### Code readability

The Tidyverse advocates also claim that the "English" in **dplyr** makes
the code easier to read.  To me, that is missing the point; as any
instructor of software engineering can tell you, the best way to make
code readable is to use REAL English, in good, meaningful code comments.

### Pipes

The Tidyverse also makes heavy use of **magrittr** *pipes*, e.g. writing
the function composition **h(g(f(x)))** as

``` r
f(x) %>%  g() %>% h()
```

Again, the pitch made is that this is "English," in this case reading
left-to-right.  But again, one might question just how valuable that is,
and in any event, I personally tend to write such code left-to-right
anyway, *without* using pipes:

``` r
a <- f(x)
b <- g(a)
h(b)
```

And much more importantly, even advocates of pipes concede that pipes
make debugging more difficult; by contrast, my style above lends itself
easily to debugging.  And again, for some problems, [piped code is
slower](https://github.com/matloff/TidyverseSkeptic/issues/10#issuecomment-511770556).
This is not a major issue, but in a long loop it would make a
difference.

Furthermore, it is my understanding that the RStudio IDE is not merely a
tool helping people to write and run code, but also is designed to
facilitate writing code using Tidyverse products in particular.  One can
click through certain **dplyr** actions, then click for input to a pipe
and so on.  To me, this makes the coding process much murkier rather
than illuminating.  It would increase the chance of undetected errors, and
leave the student with the feeling that they do not fully understand the
process, which likely is true.

### The RStudio IDE itself

Though not the Tidyverse itself, the RStudio IDE is actually another
obstacle to learning.  Granted, students do like a colorful, clickable
interface, but even the excellent online course by [R-Ladies
Sydney](https://threadreaderapp.com/thread/1119025557830684673.html),
notes that RStudio can be "overwhelming."

### The diversity claim

One of the claims made by Tidyverse advocates -- indeed for many, the
*main* claim -- is that teaching R using Tidy makes learning easier for
women and minorities.  In essence, the view is that R must be "dumbed
down" for these groups.  As a long-time ardent, active supporter of social
justice, I find this claim insulting to women and minorities, and
again, not accurate.



### Summary:  the proper status of the Tidyverse in teaching

I think it is a mistake to feature the Tidyverse in teaching R, for these
reasons:

1.  Complexity and volume.

2.  Difficulty in debugging.

3.  Inadequate generalizability.

I am certainly not saying one should only use base R; on the
contrary, CRAN is a major advantage of R, which I use extensively.
But the Tidyverse should be considered advanced R, not for beginners,
just as is the case for most complex CRAN packages.

# R's Status As an Open-Source Language

### The Long Arm of RStudio

In the [SatRday LA conference](https://losangeles2019.satrdays.org/),
April 6, 2019, a speaker who was actually explaining the *advantages* of
**data.table** in large datasets joked that package "was created by Matt
Doyle [sic].  Who's that?  No one knows who he is."  He repeated later,
"No one has ever heard of Matt Doyle."  Actually, many in the audience
had indeed heard of Matt Dowle, but in that speaker's world -- the
RStudio-educated world" -- his statement about lack of name recognition
for Matt was sadly accurate, epitomizing the impact of RStudio on the
field.

Hadley's [talk](https://t.co/7Ey27asCH4) in the 2016 useR! conference
amounted to a manifesto, calling for R to "modernize" along Tidy lines.
He conceded that this would involve "short-term pain," yet believed it
to be very much worthwhile.  Again, given RStudio's dominance, this
revolution in R was sure to follow, which it has.
  
The Tidyverse is a vigorous promotional effort by RStudio that has come
to dominate the R world.  As mentioned, the firm has been especially
active in the education realm, including gifts of funding and licensed
software, and support for conferences.  Well-intentioned and useful, to
be sure, but with the conscious effect of increasing the firm's
influence, and some would argue, power.  

A good example of the intent of RStudio to bring all of R to the Tidy
world is the **broom** package (not written by RStudio, but featured on
their Web site).  Titled "Convert Statistical Analysis Objects into Tidy
Tibbles," its goal is to convert the output of numerous packages into
Tidy form.

"Testimonials" are legion.  Non-programmers who take Tidy-based R
courses are delighted that they can now do some data analysis, and
praise the Tidyverse without realizing they have no basis for comparison 
to base-R.

RStudio counts **ggplot2** as being part of the Tidyverse, but it was
developed much earlier, and does not follow the Tidy philosophy.  But as
a result of such inclusion, I see many users who, being justly impressed
with **ggplot2**, mistakenly think that the package can only be used
from Tidy code and thus is an advantage of being Tidy.  This illustrates
the mindset that has developed.

And there is a Bandwagon Effect at work, and even a hint of cult-like
behavior.  I've seen statements on Twitter from "graduates" of Tidyverse
training who actually apologize because their code did not use the
Tidyverse.  One post I saw came from a person who panicked because she
had written a **for** loop rather than employ Tidyverse's functional
programming package **purrr**.  

And of course many who teach R want to join the Bandwagon as well, feeling
they must teach the "latest."  RStudio's educational outreach people
pitch Tidy with the aforementione testimonials.  If you were an
instructor whose students have found R difficult -- for programming IS
difficult, no matter what language and tools are used -- and are told
that Tidy is the solution, wouldn't you feel the urge to use it?  Only
the most hype-skeptics would say, "Wait a minute..."

### Adverse Impact

Tidyverse advocate Roger Peng commented in his 2018 useR! address,
"It will be interesting to see how things evolve, and whether the
community can sustain multiple ways of programming.  I think that it
can, but that's my opinion." But this seems overly optimistic.

Given the dynamics described above, we will eventually, maybe rather
soon, reach a point at which most R users will be Tidy, and have indeed
"Never heard of Matt Dowle." This will make things very difficult for
the non-Tidy R people: Non-Tidy job seekers who are excellent R coders
will find that they are dismissed out of hand by Tidy interviewers;
authors of non-Tidy CRAN code will find their contribution is considered
useless; academics submitting data science research manuscripts or grant
proposals will find that Tidy reviewers give them low scores.  In short,
R will have to bend to RStudio's wishes.

*This is classical monopolistic behavior.*  Of course, the market here,
an open source project dominated by a commercial entity, is uncharted
waters in the legal sense.  But in the (non-price) economic sense, this
is exactly what is occurring, whether a conscious goal of RStudio or
not.

Furthering accentuating the monopolistic aspect is the fact, mentioned
above, that the RStudio IDE specifically facilitates use of Tidyverse
tools.

Aside from price, again probably not an issue here, a highly insidious
consequence of monopolies is the stifling of innovation.  The case of
**data.table** discussed above is a fine case in point.

An open-source project involves a people spending a large amount of time
developing the project for free, no pay. Thus, for a commercial entity
to then swoop down and exploit all that free labor for its own profit is
fraught with peril.  To then take over the product as its own
is unconscionable.  I have no doubt that RStudio was well-intentioned in
this, sincerely believing in the Tidyverse, but many do not share this
view, and RStudio should have worked with the R Core Group (see below),
rather than taking action on its own.

The first major firm to become involved in R was Revolution Analytics
(now part of Microsoft).  There was much concern in the R community at
the time over Revo's potential negative impact on R, but instead, they
turned out to be model corporate citizens.

As noted, I know and admire the people at RStudio, *but a commercial
entity should not have such undue, unilateral influence on an
open-source project.*  

It should be noted that neither Hadley nor anyone else one from RStudio
is in the elite, 20-member R Core Group, which controls the development
of the language.  The Tidyverse thus is tantamount to an *end-run around
the leaders of this open-source project*.  As any expert in
organizational behavior will tell you, this does not augur well for the
future health of R, in spite of undoubtedly being well-intentioned.

RStudio is a great company, staffed by many talented people.  In my
opinion, their one failing has been to run off on their own, rather than
adhering to the norms of open-source projects.  

# Recommendations

In my view, RStudio can easily remedy the problem.  It can take the
following actions to greatly ameliorate the "monopolistic" problems:

1.  Promote the teaching of base-R to beginners, treating the Tidyverse
    as an advanced topic.  The popular book, *R for Everyone: Advanced
Analytics and Graphics* (second ed.), by Jared Lander does exactly this!

2.  In the various RStudio Web pages on writing fast R code, give
    **data.table** substantial coverage.
