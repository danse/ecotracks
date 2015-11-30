I really liked the chart idea from [this New York Times
article](http://www.nytimes.com/interactive/2013/10/09/us/yellen-fed-chart.html
), and i was curious to see how that would look like for my country
and the other Europeans countries.

What i see in that chart is a methophore of movement, and i was
wondering whether Europe was moving somehow in the same
direction. Clearly i expect a negative answer.

I looked for data in Eurostat, but i was not able to reduce them to an
homogeneous time interval. In the world bank data section i found what
i was looking for, data organised by year, but they covered also
almost all the countries in the world. Well, let's plot all the world
then. The user will be enabled somehow to focus on what he/she is
interested in.

## Technical aspects affecting the visualisation

### Collections of segments

I am interested in showing the evolution over time in an automatical
way, and in order to do that i converted the data in a way that
several segments can be drawn, one for each couple of year. In this
way, data can be displayed in a time dependent manner. On the other
hand, there is not a single line to interpolate easily with d3.

### Multiple indexes

The conversion script generalises the indexes. This means that it is
easy to add indicators to every point. This enables the possibility to
let the user select the interesting indexes, and see the chart nicely
adapting!

### Wrapping up

In the current situation, i imagine the final result as a simple yet
nice tool to explore those data, allowing to choose countries, time,
and indicators

On top of the time control, i imagine that it will be not so
complicated to run an animation of the paths changing over the years,
something like a play button, to put it simple

