  *This is cross posted at the
  [Scholars' Lab Blog](http://scholarslab.org/uncategorized/validating-data-with-types/).*

Recently, I had to opportunity to help [J. Warren
York](http://politics.virginia.edu/node/181 "J. Warren York"), a
graduate student in the Department of Politics here at UVa. He’s looking
at how tax law affects political contributions and advocacy, so this was
an interesting project that may tell us something useful about how the
US government works [insert your favorite broken-government joke here].

<!--more-->

To do this, he needed to download data from a number of different
sources in different formats (JSON, YAML, and CSV), pull it all apart,
and put some of it back together in a couple of new data files. One of
those sources is the [Database on Ideology, Money in Politics, and
Elections
(DIME)](http://data.stanford.edu/dime "Database on Ideology, Money in Politics, and Elections").
The data from them tells how much people and organizations have
contributed to various candidates, PAC, and other groups.

And while I’ve seen worse, it wasn’t the cleanest data file out there.
(To get an idea of what the data looks like, you can see a sample of 100
rows from this data file in [this Google
Sheet](https://docs.google.com/spreadsheets/d/1-m_8pm_s2gfpSAkMF0IYHNjKvWjqDe2d__CrbpjBxFA/edit?usp=sharing "Sample Data").)

For most projects that I’m reasonably sure that I’ll be the only
developer on, I use
[Haskell](https://www.haskell.org/ "Haskell Programming Language"). This
is a
[functional](http://en.wikipedia.org/wiki/Functional_programming "Functional Programming on Wikipedia"),
[statically
typed](http://stackoverflow.com/questions/1517582/what-is-the-difference-between-statically-typed-and-dynamically-typed-languages "Stack Overflow on What is the difference between statically typed and dynamically typed languages?")
programming language with a (partially deserved) reputation for being
difficult. However, I find that it gives me a nice balance of safety and
flexibility, of power and expressiveness.

Given Haskell’s reputation, the previous sentence probably seems to
border on insanity. Hopefully this post will prove this at least
partially correct and will highlight some of the nicer aspects of
working in Haskell. It leverages types to provide some assurances that
the data is well-formed and consistent. This means I can perform data
validation quickly and easily, and that helps everyone.

 > This post is actually runnable Haskell. If you have the
 > [GHC](https://www.haskell.org/ghc/ "GHC Haskell Compiler") compiler
 > installed you can copy and paste this post into a file, say
 > `Validating.lhs`, and run it from the command line:
 >
 > ``` {.sourceCode .bash}
 > $ runhaskell Validating.lhs contribDB_1982.csv
 > ```
 >
 > However, to follow this post, you don’t need to know Haskell. I’ll
 > try to explain enough of the concepts and syntax that matter as they
 > come up, so that anyone familiar with computer programming should be
 > able to follow along without getting into the weeds of exactly what’s
 > happening on each line.
 >
 > So first some pre-amble and boilerplate. This just makes available
 > the libraries that we’ll use.

``` {.sourceCode .haskell}
> {-# LANGUAGE OverloadedStrings #-}
>
> -- If you want more details about the code, including brief
> -- explanations of the syntax, you've come to the right place.
> -- Pay attention to the comments. This still isn't a tutorial
> -- on Haskell, but hopefully you'll have a more detailed
> -- understanding of what's happening.
>
> -- First, Haskell code is kept in modules. Executable files are
> -- in the `Main` module.
> module Main where
>
> -- Import statements make the code from these modules available
> -- in this module. Qualified imports make the code available
> -- under an alias (e.g., Data.ByteString.Lazy is aliased to B).
> import qualified Data.ByteString.Lazy as B
> import           Data.Csv
> import qualified Data.Text            as T
> import qualified Data.Vector          as V
> import           System.Environment
```

To validate the data, we just need to follow the same steps that we
would to load it. Those steps are:

1.  Define the data that you want to use;
2.  Define how to read it from a row of CSV data; and
3.  Read the input.

![Profit!](http://scholarslab.org/wp-content/uploads/2015/03/gnomes_plan.gif)

That’s it. In fact, the last item is so inconsequential that we’ll skip
it. But let’s see how the rest of it works.

Defining the Data
-----------------

First we need to define the data. We do this using *types*. If you only
know languages like Ruby, JavaScript, or Python, you may be unfamiliar
with types. Basically, they specify what your data will look like. For
example, they might specify that a `Person` data instance has a `name`
string field and an `age` integer field.

If you come from Java or C\#, you know what types are, but Haskell uses
them very differently. In Haskell, types are used to express, encode,
and enforce the requirements of your program as much as possible. The
guideline is that invalid program states should not be expressible in
the types you define. To help with that, some of the loopholes in Java’s
type system have been closed (looking at you, `null`): this makes these
specifications more meaningful. And because Haskell employs type
inference, you also don’t need to actually declare the type of every
little thing, so you get more benefit for less work.

In short, types are how we specify what data we’re interested in.

At this point in the process, programming in Haskell is a typical data
modeling exercise. But it’s also the foundation for the rest of this
post, so we’ll linger here.

Before we define the data types, we’ll first define some aliases. These
aren’t really enforced, but they make the data types that use these more
clear.

``` {.sourceCode .haskell}
> type OrgName = T.Text
> type Year    = Int
> type Amount  = Double
```

The first data type that we’ll create is `Party`. This will be similar
to enumerations in other languages, but in Haskell they’re just regular
data types. A `Party` can be either a `Dem` (Democrat), `GOP`
(Republican), `Independent`, or `Unknown`.

``` {.sourceCode .haskell}
> -- This statement says that you can make a value of type Party 
> -- using any of the constructors listed (separated by pipes).
> -- In this case, none of the constructors take extra data, so
> -- the semantics comes soley from which constructor is chosen.
> data Party = Dem | GOP | Independent | Unknown
```

We want to know what kind of entity is receiving the contribution.
However, we don’t actually care about who the recipient was: we just
want to distinguish between candidates, committees, and state-level
elections. We’ll use the `ContribEntry` data type for this information.

The following declaration states that a `ContribEntry` can be either a
`Candidate`, which must have year information and party information; a
`Committee`, which must have only a year; or a `StateLevel`, which must
have a year and a state code.

``` {.sourceCode .haskell}
> -- This shows how values are given types. `contribYear ::
> -- !Year`, says that the `contribYear` field must contain
> -- values of type `Year`. The exclamation mark tells the
> -- Haskell compiler to execute this value immediately. Unlike
> -- most other languages, Haskell will normally wait to
> -- evaluate expressions until absolutely necessary.
> data ContribEntry
>         = Candidate  { contribYear :: !Year, contribParty :: !Party }
>         | Committee  { contribYear :: !Year }
>         | StateLevel { contribYear :: !Year, stateCode :: !T.Text }
```

Each row of the data file will have information about a single
contribution made by an individual or organization. Because we’re
primarily interested in the data from organizations, this will be
collected in an `OrgContrib` data type. It will hold the organization’s
name (`orgContribName`), its district (`orgDistrict10s`), the
contribution information (`orgContribEntry`), and the amount of the
contribution (`orgContribAmount`).

``` {.sourceCode .haskell}
> data OrgContrib
>          = OrgContrib
>          { orgContribName   :: !OrgName
>          , orgDistrict10s   :: !T.Text
>          , orgContribEntry  :: !ContribEntry
>          , orgContribAmount :: !Amount
>          }
```

That’s it. We’ve now defined the data we’re interested in. On top of the
guarantees that types allow the programming language to enforce, this
exercise is also helpful because it clarifies what we want from the data
and helps us better understand the domain that we’re working in.

Data from CSV
-------------

However, we haven’t connected this data with the CSV file yet. Let’s do
that now.

To make this happen, we’ll need to take the data types that we just
defined and define instances of `FromField` for ones that are populated
from a single field, like `Party`, and `FromNamedRecord` for others,
which are built from an entire row.

`FromField` and `FromNamedRecord` are [type
classes](http://en.wikipedia.org/wiki/Type_class "Type Class on Wikipedia").
In object-oriented terms, these are similar to small interfaces, some
only declaring one or two methods. Data types can implement the type
classes that make sense, but omit the ones that do not.

In this case these type classes define what data types can be read from
a row of CSV and how that should happen.

`Party` is the first data type we’ll tackle. It only reads a single
field, so we’ll define `FromField`. In the CSV file, the data is encoded
with numeric codes, which we’ll change into `Party` values.

``` {.sourceCode .haskell}
> -- This defines a instance of `FromField` for `Party`.
> -- `parseField` is the only method. Multiple listings for this
> -- function, combined with the string literals in place of the
> -- parameter, means that the method acts as a big case
> -- statement on its one parameter. When the function is passed
> -- the string "100", the first definition will be used. The
> -- last clause, with the underscore, is a catch-all, in which
> -- the parameter's value will be ignored.
> instance FromField Party where
>     parseField "100" = return Dem
>     parseField "200" = return GOP
>     parseField "328" = return Independent
>     -- This catch-all is probably a bad idea....
>     parseField _     = return Unknown
```

Notice my comment on the next to last line. Having a catch-all field
like this introduces some [code
smell](http://en.wikipedia.org/wiki/Code_smell "Code Smell on Wikipedia"),
and it weakens the type-safety of the field. A better practice would be
to define a `Party` constructor for every numeric code and throw an
error when we find something unexpected. Since we’re only interested
here in two parties, that would be overkill, so in this case we’ll be
more flexible.

Now we can define how to read `ContribEntry` data. This is complicated
because we have to look at the value of the `recipient_type` field in
order to figure out which constructor to use.

We’ll also define a utility function, `defaulting`, that defaults empty
strings to a given value.

``` {.sourceCode .haskell}
> -- This defines the function defaulting. The first line is the
> -- type value. The definition of `defaulting` is a more
> -- complicated case statement that first tests `T.null v`
> -- (i.e., that it's empty), and `otherwise` is the "else" part
> -- of the statement.
> defaulting :: T.Text -> T.Text -> T.Text
> defaulting d v | T.null v  = d
>                | otherwise = v
>
> instance FromNamedRecord ContribEntry where
>     parseNamedRecord m = do
>         -- Read the recipient_type field. The `.:` operator
>         -- reads a specific field from the CSV row.
>         rtype <- m .: "recipient_type"
>         -- If recipient_type is empty, give it a default value
>         -- of "CAND", and then branch on that.
>         case defaulting "CAND" rtype of
>             "CAND" -> do
>                 -- Read the cycle (year) and recipient_party fields
>                 cycle <- m .: "cycle"
>                 party <- m .: "recipient_party"
>                 -- Create a Candidate
>                 return (Candidate cycle party)
>             "COMM" -> do
>                 -- Read the cycle and return a Committe
>                 cycle <- m .: "cycle"
>                 return (Committee cycle)
>             r -> do
>                 -- Everything else is a state-level contribution.
>                 -- Get the cycle and return that.
>                 cycle <- m .: "cycle"
>                 return (StateLevel cycle r)
```

(You might be wondering why I haven’t needed to define a `FromField` for
`Year` for the “cycle” fields. Remember that `Year` is just an alias for
`Int`, and the CSV library already defines `FromField` for the `Int`
type.)

We can finally define the instance for `OrgContrib`. After the
complexity of `ContribEntry`, this one will be much simpler. We’ll
extract the values for a few fields, parse the `ContribEntry`, and then
create and return the `OrgContrib` value.

``` {.sourceCode .haskell}
> instance FromNamedRecord OrgContrib where
>     parseNamedRecord m = do
>         name     <- m .: "contributor_name"
>         district <- m .: "contributor_district_10s"
>         contrib  <- parseNamedRecord m
>         amount   <- m .: "amount"
>         return (OrgContrib name district contrib amount)
```

With these in place, we can read the data and have it verified at the
same time. For example, if the file reads correctly, I know that the
`Year` data are integers and that `Party` fields contain valid data.

And that’s really all there is to it. Below the end of the article, I’ve
included a function to read the CSV data from a file and the `main`
function, which controls the whole process. However, reading and
validating the data has already been taken care of.

Of course, while these types provide reasonable validation, you could
get much better, depending on how you define your types and how you
parse the incoming data. (For example, you could only allow valid state
codes for `StateLevel` or limit years to a given range.)

If you’re wondering about tests, the implementations of `FromField` and
`FromNamedRecord` would be good to have tests for. However, the parts of
the program’s requirements that are enforced in the types don’t really
need testing; for example, I wouldn't test that party fields will always
be parsed as a `Party`.

Types also come in handy in other circumstances: when you’ve left the
code for a while and need to get back into it, they provide a minimum
amount of guidance; and when you need to refactor, they act as a
base-line set of regression tests, to tell you when you’ve broken
something.

Overall, I find that this small program shows how Haskell can provide a
lot of power and expressivity for relatively little code.

But the immediate benefit in this case is that I was able to provide
John more assurances about his data, and to provide them more quickly.
It’s a nice example of leveraging types to write better programs that
provide real-world benefits.

The full code for this project is in my
[popvox-scrape](https://github.com/erochest/popvox-scrape "The Popvox Scrape Repository")
repository. Feel free to check it out.

------------------------------------------------------------------------

``` {.sourceCode .haskell}
> readData :: FilePath -> IO (Either String (Header, V.Vector OrgContrib))
> readData filename = do
>     rawData <- B.readFile filename
>     return (decodeByName rawData)
>
> main :: IO ()
> main = do
>     args <- getArgs
>     case args of
>         [filename] -> do
>             dataRows <- readData filename
>
>             case dataRows of
>                 Left err -> putStrLn ("ERROR: " ++ err)
>                 Right (_, rows) -> putStrLn (  "SUCCESS: "
>                                             ++ show (V.length rows)
>                                             ++ " read.")
>
>         _ -> putStrLn "usage: runhaskell Validate.lhs data-file.csv"
```
