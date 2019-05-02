---
title: Disturbances amplify tree community responses to climate change in the temperate-boreal ecotone
documentclass: article
font: 12pt
papersize: a4paper
geometry: margin=1in
header-includes:
    - \usepackage{setspace}
    - \setstretch{1,5}
    - \usepackage{lineno}
    - \linenumbers
    - \raggedright
---

**Running title**: Tree community responses to climate change

# Abstract

**Aim**
Climate change causes major shifts in species distributions, reshuffling
community composition and favoring warm-adapted species ("thermophilization").
Tree community response is likely to be affected by major disturbances such as
fire and harvest. Here, we quantify the relative contributions of climate change
and disturbances to temporal shifts in tree composition over the last decades
and evaluate whether disturbances accelerate community thermophilization.

**Location** Québec, Canada

**Time period** 1970-2016

**Taxa studied** Trees

**Methods**
Using 6281 forest inventory plots, we quantified temporal changes in species
composition between a historical (1970–1980) and a contemporary period
(2000–2016) by measuring temporal ß diversity, gains and losses. The effects of
climate and disturbances on temporal ß diversity were quantified using multiple
regressions and variation partitioning. We compared how community indices of
species temperature preference (CTI) and shade tolerance (CSI) changed for
forests that experienced different levels of disturbance. We quantified the
contribution of species gains and losses to change in CTI.

**Results**
Temporal ß diversity was mainly driven by disturbances, with harvesting as the
most important factor. Despite the prevailing influence of disturbances, we
revealed a generalized thermophilization ($\Delta$CTI&nbsp;=&nbsp;+0.03°C/decade) of
forests in Québec. However, this shift in community composition was weakly
explained by climate change and considerably slower than the rate of warming
(+0.14°C/decade). Importantly, thermophilization was amplified by moderate disturbances
(+0.05°C/decade), a five-fold increase compared to minor disturbances
(+0.01°C/decade). The gains and losses of few tree species contributed to
this community-level shift.

**Conclusions**
Our study provides evidence that disturbances can strongly modify tree community
responses to climate change. Moderate disturbances, such as harvesting, may
reduce competition and facilitate gains of warm-adapted species, which then
accelerate thermophilization of tree communities under climate change. However,
this synergistic effect only benefited a few species, such as maples. Although
accelerated by disturbances, tree responses still lag behind climate change.


## Keywords

Beta diversity,
Climate change,
Community temperature index,
Community temporal change,
Disturbances,
Forest,
Québec,
Temperate-boreal ecotone,
Thermophilization.

\pagebreak

# Introduction

Climate warming over the past century has led to distribution shifts in many
species [@parmesan_globally_2003]. Despite the general trend of poleward and
upward (in altitude) range shifts, the timing, magnitude and even direction of
species shifts vary considerably among taxa and regions [@vanderwal_focus_2013].
Major reshuffling of community composition is therefore expected. Yet, we lack
an understanding of the community-level consequences of climate-driven shifts.
This knowledge gap is even greater in forests where tree species response is
slow [@sittaro_tree_2017] relative to the short duration of typical ecological
studies (only a few years). So far, much of the emphasis has been placed on
detecting species shifts at their range edge, where early signs of changes are
expected to be more easily detectable [@jump_altitude-for-latitude_2009]. As
such, there is a growing body of evidence for contemporary shifts in tree
species distributions along altitudinal gradient in mountains
[@beckage_rapid_2008; @savage_elevational_2015; @lenoir_significant_2008], where
ecotones are narrow and well-defined [@jump_altitude-for-latitude_2009]. Similar
evidence is also beginning to emerge for latitudinal shifts
[@fisichelli_temperate_2014; @sittaro_tree_2017]. Though, because of the focus
on range shifts, there has been little empirical work on the effect of climate
change on tree community composition and abundance distribution within species
range [e.g. @esquivel-muelbert_compositional_2018; @searle_persistent_2017].

Worldwide increases in tree mortality rates triggered by droughts and heat
stress have been documented recently [@allen_global_2010]. In the long term,
even minor changes in demographic rates can modify the balance between local
species gains and losses, leading to temporal change in community composition.
Yet, as trees are long-lived species, mortality and recruitment rates are low
[@iverson_tree-species_2013]. Thus, tree community responses to contemporary
climate warming are likely to be lagged, resulting in extinction debts
[@talluto_extinction_2017; @svenning_disequilibrium_2013], as opposed to
short-lived herbaceous plants characterized by high community turnover
[@beckerscarpitta_four_2019; @savage_elevational_2015]. Consequently,
community-level response to climate change remains difficult to quantify and is
probably underestimated at the moment.

Furthermore, in northern temperate and boreal regions, natural disturbances
(fires and insect outbreaks) and anthropogenic disturbances (timber harvesting)
are major drivers of tree community dynamics [@gauthier_vulnerability_2015;
@boucher_fire_2017]. These pulse disturbances are likely to dominate local,
short-term biotic changes, resulting in increased prevalence of young forests
dominated by early successional species. These short-term effects could easily
mask climate-induced changes that are expected to occur on much longer time
scales and broader spatial scales. For this reason, disturbances are often
considered as inconvenient confounding factors instead of an inherent part of
contemporary ecosystems. Thus, numerous studies have searched for trends in
relatively undisturbed systems [@parmesan_globally_2003] rather than accounting
for their effects. Yet, disturbances and climate change have a high potential
for interactions, which can lead to synergistic or antagonistic ecological
effects that are difficult to predict [@brook_synergies_2008]. Indeed,
disturbances create canopy openings that could facilitate the northward
migration of temperate species [@xu_importance_2012; @vanderwel_how_2014;
@leithead_northward_2010]. In addition, the frequency and intensity of natural
disturbances can increase as an indirect effect of climate change
[@seidl_forest_2017].

Positive feedback between disturbances and climate change have already been
observed locally on long temporal scales, over decades to centuries. For
example, comparison of early industrial (early 1900) to contemporary forests in
the Bas-Saint-Laurent region of Québec showed that logging practices turned
old-aged conifer forests into young mixed and deciduous forests
[@boucher_logging-induced_2006; @boucher_logging_2009]. @leithead_northward_2010
also observed that the establishment of southern temperate species in the
temperate-boreal ecotone of northern Ontario increased with the size and age of
canopy gaps. Conversely, @woodall_assessing_2013 found little influence of
disturbances on tree range limits in northern US during a 5-year interval from
1998 to 2003. Hence, there is still no clear consensus that positive synergy
could influence contemporary tree community response at a large spatial scale.
Yet, to anticipate and adapt to future forest changes, large-scale empirical
studies are required in order to understand individual and aggregated impacts of
multiple stressors on forest composition.

Even though disturbances may mask slow community responses to climate change,
these two drivers leave distinguishable signatures on communities. Climate
warming should favor warm-adapted species at the expense of cold-adapted
species, leading to a "thermophilization" of communities
[@savage_elevational_2015; @de_frenne_microclimate_2013]. Conversely,
disturbances should increase the prevalence of young forests dominated by
shade-intolerant species [@savage_elevational_2015; @boucher_fire_2017]. Hence,
analyzing shifts of relevant functional traits in communities using large-scale
monitoring data should unravel the role of different environmental drivers in
shaping communities [@violle_let_2007]. For instance, the Community Temperature
Index (CTI) has been used to measure thermophilization in various communities,
such as plants, trees, birds and fishes [@devictor_birds_2008;
@gauzere_rapid_2015; @de_frenne_microclimate_2013; @feeley_compositional_2013;
@beckerscarpitta_four_2019; @cheung_signature_2013]. The CTI is a
community abundance-weighted average of the Species Temperature Indices
(STI; proxy for species thermal preference computed as the mean
temperature of a given species distribution). Because CTI reflects the relative
abundance of warm-adapted (high STI) vs cold-adapted species (low STI), it is
expected to increase following climate warming if species are responding
according to their temperature requirements.

Here, we quantify the temporal shifts in tree community composition in the
temperate-boreal ecotone, and test whether recent climate change is impacting
forest tree composition. We analyzed data from a long-term forest inventory
program across meridional Québec, where vegetation ranges from northern hardwood
forests dominated by sugar maple at low latitudes (up to 47°N) to mixed forests
dominated by balsam fir (from 47°N to 48°N), to boreal forests dominated by
black spruce at high latitudes (from 49°N to 52°N). Such dataset allowed us to
compare community responses to recent climate change in plots that experienced
different levels of disturbances along a large latitudinal gradient. As such, we
can address four questions: (1) how has the composition of forest communities
changed during the last decades across different biomes? (2) What is the
relative contribution of climate change and disturbances to these temporal
community changes? (3) Have forest communities experienced a thermophilization
during the last decades? And can disturbances accelerate community
thermophilization? (4) How do gains and losses of specific tree species
contribute to thermophilization?

Specifically, we measured temporal ß diversity [@legendre_temporal_2019] over
6000 resurveyed communities between a historical (1970–1980) and a contemporary
(2000–2016) period. Temporal ß diversity, which describes the temporal
dissimilarity in community composition, was decomposed into gains and losses, to
investigate the underlying mechanisms of change. Then, we quantified the
effects of climate change and disturbances on temporal ß diversity using
multiple regressions and variation partitioning. Using community indices for
temperature (CTI) and shade tolerance (CSI), we quantified community-level
changes associated with thermophilization and succession and compared these
changes among levels of disturbances. We finally quantified the species-specific
contributions to thermophilization.


# Methods

## Study area

To analyze large-scale temporal changes in forest community composition, we used
Québec forest inventory plots, which have been sampled south of the 52^nd^
parallel since 1970 by the Ministère des forêts, de la Faune et des Parcs
[Fig. 1; @mffp_placettes-echantillons_2016]. We compared the first and last
years of the inventory and selected plots that were sampled both before 1980 and
after 2000 so as to maximize the time interval. We disregarded plots that were
subjected to active reforestation during the study period because we were
interested in compositional changes resulting from natural post-disturbance
recolonization. We also eliminated plots without trees (due to a disturbance)
either at their first or last year of sampling. This yielded a total of
6281 plots (Fig. 1) analyzed with a median of 35 years between
surveys (1st quartile: 33 and 3rd quartile: 41 years).

Within each circular plot (400 m^2^), trees larger than 9 cm in
diameter at breast height (DBH) were identified to species, measured and their
vitality noted [@mffp_placettes-echantillons_2016]. The selected plots included
47 tree species (Table S1), which were all included in the analyses because even
the rarest can contribute to temporal changes; their identity does not bias our
analyses and, contrary to mobile species, there is very little detection bias in
tree surveys.

## Environmental variables

The annual past climatic conditions, covering a period from 1960 to 2013, were
extracted using a 2 km^2^ (60 arc sec) resolution grid for the entire study area
using the ANUSPLIN climate modeling software
[http://cfs.nrcan.gc.ca/projects/3/8; @mckenney_customized_2011]. Bioclimatic
variables hypothesized to influence tree survival were intercepted at plot
locations: the mean temperature and total precipitation during the growing
season, minimum temperature of the coldest period, maximum temperature of the
warmest period and the annual climate moisture index (CMI). From these
bioclimatic variables, we derived different predictors (see Table 1 for
definitions). Over the past four decades, growing season temperature and
precipitation have increased by 0.14 °C/decade and 9.5 mm/decade, respectively,
while CMI has decreased by 1.2 mm/decade (Fig. S1).

To analyze the influence of disturbances, we collected information about fire,
insect outbreak, windfall and harvesting from the forest inventory dataset
[@mffp_placettes-echantillons_2016]. At each plot measurement, disturbance type
(23 types) and intensity were recorded. We reclassified disturbances according
to four main types (fire, insect outbreak, windfall and harvesting), with 3
levels of intensity each (none, moderate or major; Table 1, Fig. S2). Major
disturbances correspond to natural or anthropogenic events that have eliminated
more than 75% of the total tree basal area of the previous forest stand, while
moderate disturbances have eliminated between 25% and 75%. Core samples were
also collected on selected trees during surveys to measure their age. Stand age
was estimated as the mean of these measures to account for forest succession
processes after disturbances. Finally, as time interval between the first and
last measurements varies among the forest plots, it was included as a predictor.

## Analysis

### ß diversity

For each plot, we computed temporal ß diversity [@legendre_temporal_2019], which
is the difference in species composition between two surveys of a given plot, by
comparing local tree abundance in forest plots between the historical (before
1980, $t_1$) and contemporary (after 2000, $t_2$) periods. This dissimilarity
index (ß) was computed using the Ružička coefficient, which is one of the
quantitative forms of the Jaccard dissimilarity:

$\beta = (B+C)/(A+B+C)$ where, for $n$ species:

$A = \sum_{j=1}^n a_j$ : unscaled similarity &mdash; $a_j$ represents the number
of individuals of species $j$ that is common between $t_1$ et $t_2$ ; 

$B = \sum_{j=1}^n b_j$ : unscaled species abundance losses &mdash; $b_j$
represents the number of individuals of species $j$ present at $t_1$ but not at
$t_2$ ; when species $j$ gained in abundance, $b_j$ = 0;

$C = \sum_{j=1}^n c_j$ : unscaled species abundance gains &mdash; $c_j$
represents the number of individuals of species $j$ present at $t_2$ but not at
$t_1$ ; when species $j$ decreased in abundance, $c_j$ = 0;

This temporal ß diversity varies from 0 (community compositions at $t_1$ and
$t_2$ are exactly the same) to 1 (communities have no shared species). The use
of this dissimilarity index enabled us to decompose the compositional change
into relative gains ($C/(A+B+C)$) and losses ($B/(A+B+C)$) in tree abundance.
Throughout this paper, gains and losses refer to these relative metrics.

This additive framework allowed us to partition further the different components
contributing to ß diversity. Temporal dissimilarity in tree community can be
decomposed into the dissimilarity (gains and losses) of different species groups
of interest, here boreal (9 species), pioneer (11 species) and temperate species
(27 species; Table S1). The temporal dissimilarity of a given group, for
instance boreal, relative to all species is simply: $\beta_{boreal} =
(B_{boreal}+C_{boreal})/(A+B+C)$, with $(A+B+C)$ the denominator computed over
all tree species. As a consequence,

$$\beta = \beta_{boreal} + \beta_{pioneer} + \beta_{temperate}$$

### Assessing relative importance of drivers of community changes

We evaluated the effects of multiple drivers on temporal ß, gains and losses
using multiple regressions, which can be combined with variation
partitioning analyses [@borcard_partialling_1992]. Because the dependent
variables (ß, gains, losses) are in the standard unit range [0, 1], we used a
logit transformation $y'=log(y/(1-y))$.

We next quantify the influence of climate change and disturbances, while
controlling for the baseline climate gradient and different time interval. To do
so, we classified our predictor variables in three subsets: baseline conditions,
climate change and disturbances (Table 1). We then generated global models
predicting ß, gains and losses, for each of the three subsets. We also tested
relevant interactions between disturbance and climate predictors (fire *
$\Delta$CMI; outbreak * $\Delta$Temp; harvest * $\Delta$Temp). A forward
selection of explanatory variables based on two stopping criteria [$\alpha$ and
global $R^2_{adj}$; @blanchet_forward_2008] was performed to obtain parsimonious
regression models for each of the three subsets. The predictors had been
previously standardized to z-scores to allow comparison of their slope
coefficients and we ensured that residuals met assumptions of normality and
homoscedasticity.

We assessed the unique contributions of each predictor subset - baseline
conditions, climate change and disturbances - as well as their shared effect on
forest community changes using variation partitioning analysis on parsimonious
models. The significance of each unique fraction was tested using 9999
permutations.

### Functional index of community change

To test whether or not climate warming contributed to community changes, we
combined climate and tree occurrence data to measure a Community Temperature
Index (CTI) from species temperature distributions [@devictor_birds_2008].
Specifically, we overlaid interpolated climate data [mean annual temperature
averages from 1970–2000 at a spatial resolution of 1 km^2^, available online
http://worldclim.org/version2; @fick_worldclim_2017] and occurrence data from
multiple forest inventory databases of eastern North America (collected in the
QUICC-FOR project; https://github.com/QUICC-FOR) for all studied tree species.
The mean annual temperature for each occurrence was extracted to infer species
temperature distributions. The mean of these temperature values was used as a
proxy for a species thermal preference (Species Temperature Index, STI, in
degrees Celsius; Table S1). For each plot in each time period, CTI was then
calculated as the mean of the STI weighted by the abundances of the species
present in that plot. To evaluate the directionality of the changes in
communities between the historical ($t_1$) and contemporary ($t_2$) periods, we
computed the temporal shift in CTI per decade:

$$\Delta CTI = \frac{CTI_{t2} - CTI_{t1}}{t_2 - t_1} \times 10$$

A positive value of $\Delta$CTI indicates an overall thermophilization of the
tree community in degrees Celsius per decade.

To evaluate whether community thermophilization was caused by decreases of
cold-adapted species or increases of warm-adapted species, we quantified the
shift in the left (10^th^ percentile) and right tails (90^th^ percentile) of the
plot-level distribution of species temperature preferences [see Fig.1 in
@de_frenne_microclimate_2013]. To do so, for each individual of a species
present in a plot, we sampled 1000 temperature values from the temperature
distribution of that species and summed the values across the individual stems
present to obtain a plot-level temperature distribution. Contrary to
@de_frenne_microclimate_2013, we used the entire distribution for each species
instead of modeling species thermal response curves because numerous species
were not following a Gaussian distribution; in particular, many southern species
presented bimodal and skewed distributions. Then, we calculated the 10^th^ (cold
tail) and 90^th^ percentiles (warm tail) of the plot-level temperature
distributions. The shifts in the cold and warm tails of the temperature
distributions were computed in the same way as for the shifts in mean. A
positive shift of the cold tail indicates a decrease of cold-adapted species,
while a positive shift of the warm tail indicates an increase of warm-adapted
species; both result in thermophilization.

We also quantified how each species contributed to $\Delta$CTI through gain or
loss in abundances. Species contributions were assessed following these steps:
for each species, 1) we replaced the abundance at $t_2$ by its previous
abundance at $t_1$, as if the species abundance had not changed over time; 2) we
computed a new $CTI_{t2}'$; 3) then we calculated $\Delta$CTI' using $CTI_{t2}'$
and $CTI_{t1}$ as above; and finally 4) we measured the difference between
$\Delta$CTI' and $\Delta$CTI in each plot. A positive value indicates that the
change (gain or loss) of a given species abundance increases thermophilization
in a plot. We also determined the role of species gains and losses in
$\Delta$CTI by averaging their contributions for plots where they increased and
where they decreased.

To test the alternative hypothesis that community changes are resulting from
post-disturbance succession, we collected traits about shade tolerance
[@niinemets_tolerance_2006] to compute a Species Shade Index (SSI) that
represents a species ability to grow in shade conditions. Shade tolerance
indices ranged from 1 (very intolerant to shade) to 5 (very tolerant) on a
continuous scale. As for CTI, a Community Shade Index (CSI) was computed for
each plot as the mean of the SSI values weighted by the abundances of the
species present in that plot. Temporal shift in CSI, $\Delta$CSI, was computed
in the same way as for $\Delta$CTI, where a positive value indicates a progress
in stand succession toward climax, in unit per decade.

All analyses were performed using the R programming language version 3.5.1
[@r_core_team_r_2018]. The list of R packages that have been used throughout the
analysis is provided in Table S2. R scripts will be made available on GitHub
upon manuscript acceptance.


# Results

## Temporal ß diversity

Mean temporal ß diversity was 0.56 for the whole study area (*n* = 6281), and
these temporal changes in community composition were attributable to slightly
more gains in abundances (52.5%) than losses (47.5%; Fig. 2a). Temporal ß
diversity varied along a latitudinal gradient; it tended to decrease northward,
reaching its maximum at 48°N of latitude, which corresponds to the northern
limit of the balsam fir-yellow birch domain, the ecotone between boreal and
deciduous forests. North of the 49°N of latitude, in the spruce-moss domain,
temporal ß changes were dominated by losses whereas, south of this limit, losses
prevailed.

For undisturbed forests, compositional changes were particularly notable in the
southernmost domains (sugar maple-hickory and sugar maple-basswood; ß = 0.54)
and remained quite high from the sugar maple-yellow birch (ß = 0.46) up in the
balsam fir-white birch (ß = 0.49), whereas the northernmost domain (spruce-moss)
changed very little (ß = 0.32; Fig. 3). This pattern reflects the latitudinal
variation in the contribution of the three species groups to temporal ß (Fig.
2b). Community changes were mainly determined by gains in temperate species
south of 47°N and by gains in boreal species north of 47°N (where boreal species
are the most abundant species group). Some species have experienced great
changes in abundance and occurrence throughout these domains, mainly black
spruce, red maple, yellow birch, American beech and trembling aspen, and likely
contributed largely to the pattern of temporal ß diversity (Fig. S3).

The magnitude of compositional change in forests was highly influenced by
disturbances (Figs 2b-d, 3, S4). The mean temporal ß was 0.54 for
moderately disturbed forests and 0.75 for highly disturbed forests, whereas it
was only 0.43 for undisturbed forests (all domains combined). Yet, the fraction
of changes attributed to losses was rather similar among the three levels of
disturbance (none: 42.4%; moderate: 48.8%; major: 49.7%), except for spruce-moss
forests where moderate and major disturbances resulted in considerably more
losses than gains (71.9% and 63.7% respectively).

Disturbances increased both gains and losses, and therefore ß
diversity, in each species group (Fig. 2b-d). At high disturbance level, we
observed a strong surge in turnover of boreal tree species (both gains and
losses) along with an increase in gains of pioneer species. In contrast,
turnover in temperate species was slightly higher at moderate disturbance levels.


## Drivers of temporal changes

Once combined, predictors from the three subsets (baseline, climate change and
disturbances) explained together 32% of the variation of temporal ß diversity,
21% of gains and 17% of losses (Fig. 4).

Community temporal changes were mainly driven by disturbances ($R^2_{adj}$ for
ß: 23%; gains: 12%; losses: 18%), whereas the impacts of climate change were
significant but comparatively modest ($R^2_{adj}$ less than 1%; Fig. 5d-f).
Overall, disturbances enhanced compositional changes, with major harvest being
the most important driver. Interestingly, while most disturbances promoted
losses and reduced gains, intense fires had the opposite effect (Fig. 4b-c). As
time-since-disturbance increases and the forests grow old (Age), forest
composition changes less and colonization of new individuals becomes scarcer
(Fig. 4a-b).

Regression models provided only weak evidence of climate change effect on forest
community change. Mainly, extreme minimum CMI (CMI min, i.e. drought) and
extreme cold (Temp min) contributed to community changes through losses in tree
abundances (Fig. 4a,c). Increase in precipitation ($\Delta$Precip) favored
tree gains. Although the effect of climate warming alone ($\Delta$Temp) was
weak, we found strong interactions with disturbance variables. Increasing
temperature amplified the effect of harvesting on losses and gains but mitigated
the effect of insect outbreaks. Variables related to baseline conditions were
more important than climate change, reflecting a latitudinal gradient in
community change.

## Changes in community temperature and shade indices

The community temperature index (CTI) increased significantly between the
historical and contemporary period (paired t-test *p*-value < 0.001; mean of
+0.03 °C/decade for all plots, ranging from -0.02 to +0.05 across domains),
which indicates a generalized community thermophilization throughout the study
area. During the same period, the community shade index (CSI) also increased
(+0.01 unit/decade), suggesting a transition towards late successional forests.

Thermophilization was significantly larger in moderately disturbed forests
($\Delta$CTI = +0.05 °C/decade) than in undisturbed (+0.01 °C/decade) or highly
disturbed forests (+0.02 °C/decade; Anova F(2, 6278) = 13.05, *p*-value < 0.001;
post-hoc Tukey tests showed that moderately disturbed forests had significantly
higher $\Delta$CTI than the other two levels). Moreover, the thermophilization
in moderately disturbed forests extended further north, up to 48°N, in the
balsam fir-white birch domain (Fig. 5b,e). Despite the influence of disturbances
on thermophilization, change in CTI was weakly explained by our environmental
predictors ($R^2_{adj}$ ca. 2% for all predictors in Table 1). Moreover, the
relationship between thermophilization and climate change predictors was
surprisingly weak ($R^2_{adj}$ < 1%), with no correlation at all with
temperature change.

The analysis of $\Delta$CSI revealed that the higher the level of disturbances
the more pronounced the decrease of CSI was (Fig. 5a-c), which was consistent
with the gains in pioneer species (Fig. 2). The disparity between the
$\Delta$CTI and $\Delta$CSI curves shown in Fig. 5, as well as the weak
correlation between STI and SSI (Pearson *r* = 0.15, *p*-value = 0.26), suggest
that thermophilization was not trivially driven by successional processes.

Community thermophilization was asymmetrical, with larger increases in the
warm-tail of the temperature distributions than in the cold-tail
(Fig. 5d-f), indicating that thermophilization was mainly driven by larger
gains in warm-adapted species. The positive correlation between
$\Delta$CTI and gains in temperate species in all domains also corroborates this
finding (Fig. S5). Moderate disturbances exacerbated this effect from the
sugar maple-yellow birch up to the balsam fir-white birch domain (larger
increase in the warm tail; Fig. 5e).

Only a few species contributed substantially to the community thermophilization
(Fig. 6). Gains of red maple and sugar maple, as well as losses of balsam fir
and black spruce, contributed strongly to the thermophilization of all forests.
In addition to the change of these four species, the losses of white birch and
white spruce also played a key role in the thermophilization of ecotonal forests
in the balsam fir-yellow birch. Moreover, temperate species such as American
beech, red oak and white ash contributed mostly to the thermophilization of
southern domains (Fig. 6) where their abundance has increased (Fig. S3). In
contrast, the surge in CTI north of the 49°N (spruce-moss) in highly disturbed
forests (Fig. 5) was likely due to the replacement of boreal species by pioneer
species (Fig. S5), such as white birch and willow spp. (Fig. 6).


# Discussion

In this study, we found evidence of climate‐induced shifts in tree community
composition despite the prevailing influence of disturbances. Local and
short-term influence of disturbances masks long-term and lagging climate-induced
changes in community. Yet, we revealed a generalized thermophilization of
forests throughout the temperate-boreal ecotone of Québec, driven by a
concurrent gain of temperate species and loss of boreal species. Moreover, we
found that moderate disturbances likely accelerated thermophilization. Hence,
moderate disturbances, but not major ones, could facilitate gains in
warm-adapted species under climate change. Taken together, our results suggest
that disturbances strongly modify tree community responses to climate
change, revealing potential synergies that are yet to be studied.

## Impact of disturbances on tree community changes

Our results suggest that disturbances (e.g., clear-cutting, insect outbreaks,
fires) are the primary drivers of forest community changes in the
temperate-boreal ecotone. Such findings are in agreement with previous work
showing that disturbances alter rapidly and profoundly tree communities that
otherwise respond slowly to environmental changes [@vanderwel_quantifying_2013].

Furthermore, this study underscores the prevalence of logging activities on the
forest dynamics of the temperate-boreal ecotone. Tree harvesting was not only
the type of disturbances with the highest impact on community changes, but also
the most frequent: 24.2% (1521 out of 6281) of the plots were cut during the
study period, which represents more than the frequency of insect outbreak, 20.3%
and fire, 4.4% (Fig. S2). Natural disturbances and tree harvesting, however,
impact forest composition and structure very differently. It has been shown that
tree harvesting favored early successional stands to the detriment of old-growth
forests [@boucher_logging_2009; @boucher_impact_2012]. Such changes in forest
age structure and composition jeopardize both forest resilience
[@cyr_forest_2009; @grondin_have_2018] and wildlife that only thrives in mature
forests [@tremblay_harvesting_2018].

## Climate-induced change in tree community

Our findings highlight an ongoing shift toward more warm-adapted tree species in
forests across the temperate-boreal ecotone. This overall thermophilization
trend of tree communities is consistent with the hypothesis of climate-induced
range shift, expanding on earlier findings that forests are responding to
climate warming [e.g. @sittaro_tree_2017; @leithead_northward_2010;
@fisichelli_temperate_2014]. However, the observed increase of tree community
temperature of +0.03 °C/decade is considerably smaller than the rising trend in
growing season temperature of 0.14 °C/decade (Fig. S1). This corroborates the
conclusion of numerous studies that tree responses often lag behind
environmental changes [@sittaro_tree_2017; @talluto_extinction_2017;
@renwick_temporal_2015; @svenning_disequilibrium_2013], including recent studies
showing no statistically significant response [@beckerscarpitta_four_2019]. The
inertia of forest communities suggests ongoing extinction debts and colonization
failures due to tree high longevity and low dispersal ability
[@svenning_disequilibrium_2013]. Considering the velocity of predicted future
climate change, this disequilibrium is likely to grow, leading to biodiversity
losses.

## Feedback between climate change and disturbances

Our most striking finding is that community thermophilization was amplified by
moderate disturbances. Our combined analysis of change in CTI and CSI also
allowed us to disentangle effects from climate change from successional
processes, highlighting that the observed thermophilization was not simply
correlated with the replacement of boreal by pioneer species. Our work provides
a broad-scale community perspective on the role played by disturbances in
promoting northward migration of tree species that is in agreement with the
conclusions of recent empirical [@leithead_northward_2010;
@boucher_logging-induced_2006] and simulation [@wang_importance_2015;
@vanderwel_how_2014] studies.

Disturbances likely accelerate forest changes by reducing competition and
providing establishment opportunities to warm-adapted temperate tree species
[@leithead_northward_2010; @svenning_disequilibrium_2013]. Indeed, in the
absence of disturbance, trees grow slowly, their mortality rates are low and
competition for space and light is strong, thus preventing warm-adapted species
from colonizing, despite the suitability of climatic conditions; community
thermophilization is consequently very slow. Moderate disturbances, however,
remove resident species and reduce competition, which enhances the replacement
of boreal by temperate trees, thereby increasing the thermophilization rate. In
contrast, major disturbances only favor early successional species. Such
findings echo the well-known intermediate disturbance hypothesis
[@connell_diversity_1978]; as in the classical hypothesis, intermediate
disturbances lower interspecific competition but here, instead of having a
positive effect on local species richness, it accelerates ecological transition.

Furthermore, disturbances may also influence the compositional shift by
modifying local abiotic conditions. For instance, it has been shown that forest
canopy closure attenuates thermophilization of understory communities, likely by
maintaining cool and wet local microclimates [@de_frenne_microclimate_2013;
@stevens_forest_2015]. Therefore, as disturbances create canopy openings, they
affect local microclimates which likely alter the survival rates of tree
saplings as well as the understory plant communities.

Our complete set of predictors poorly explained the observed forest
thermophilization, likely because this process was highly variable among
localities. Forest composition is thus changing as expected under climate
warming, but thermophilization does not appear to be directly driven by rising
temperatures. As suggested by @renwick_temporal_2015, we surmise that, as
climate warms up, moderate disturbances could foster punctuated and episodic
migration of warm-adapted species in localities where conditions are otherwise
favorable. However, it raises questions about the specific conditions in which
the thermophilization process can effectively take place. Further analyses are
required in order to determine which factors can trigger (e.g. type, size,
frequency of disturbances) or constrain (e.g. soil type, competition,
precipitation) the invasion by warm-adapted species.

## Species contribution to community thermophilization

We found that the observed community thermophilization was caused by gains and
losses in abundance of a restricted group of species which indicates that
several species lag even more behind climate change. This differential rate of
species response also entails that larger reshuffling of communities are still
ahead. The interaction between climate and disturbances likely promotes
generalist tree species adapted to disturbance with high dispersal abilities
[@aubin_traits_2016]. For instance, generalist species like maples, especially
red maple, have been expanding in eastern North America since the pre-industrial
period [@thompson_four_2013; @boucher_fire_2017] and recently established
themselves in boreal forests [@sittaro_tree_2017; @leithead_northward_2010]
because they quickly take advantage from disturbances and thrive in a wide
variety of ecological conditions. In contrast, some species limited by
dispersal, such as hickories and basswood, or constrained to specific habitat,
such as silver maple, might not benefit from these opportunities.


## Long-term perspectives for the temperate-boreal ecotone

Although the time period covered by our study (46 years) is sufficient to
observe significant trends in compositional changes in forests, it is not long
enough to test whether warm-adapted temperate species will persist and thrive in
these novel assemblages or if boreal species will out-compete them in the long
run. Therefore, an important question remains: does the current forest
thermophilization indicates an ongoing state shift or only a transient dynamic?
Multiple studies suggest a persistence of these novel assemblages. For instance,
after a century of logging disturbances, temperate species were found to have
increased and persisted in forests formerly dominated by conifers
[@boucher_logging-induced_2006]. Furthermore, @frechette_evidence_2013 provided
evidence that the northern limit of the temperate biome was located about 500 km
north of its actual limit during the last interglacial period (6-7°C warmer),
suggesting that a northward shift of the ecotone is possible. Hence, while
climate warming erodes forest resilience by affecting competitive advantages and
generating colonization debt, our results demonstrate that moderate disturbances
play a major role in promoting regime shift by speeding up the transition from
one ecosystem state to another [@johnstone_changing_2016].

We show that the ecotonal zone is particularly sensitive to the combination of
climate warming and moderate disturbances, where their interaction is likely to
promote a patchy northward shift of the ecotonal zone into the balsam fir-white
birch domain. Yet, change in composition may directly affect timber supply,
especially in the ecotone where deciduous species replace conifers, and
indirectly affect natural disturbance regime. Thus, major changes in forest
management goals will become necessary to maintain ecological functions and
processes [@williamson_climate_2009; @daniel_incorporating_2017]. Our study
provides valuable insights for potential adaptation measures in forest
management. To help forest keep pace with climate change, planned selection
cutting could be used to reduce competition in certain areas and accelerate
species range shifts. Such management is however a double-edged sword as it
could bridge the gap between current species distribution and their optimal
climate niche, but it could at the same time decrease forest diversity by
promoting a narrow set of species. Hence, a combination of selection cutting
with assisted migration might be necessary to maintain diverse forests
[@iverson_tree-species_2013].

The frequency and severity of natural disturbances, such as fires, insect
outbreaks, droughts and heat waves, are expected to increase in many regions of
the world [@seidl_forest_2017; @bergeron_past_2006]. In the light of our
results, this may lead to greater forest compositional changes over the next
decades and potentially to permanent ecosystem shifts [@grondin_have_2018].
However, if disturbances becomes too intense, they could merely promote
early-successional pioneer species, thereby impoverishing forest communities.
Our study focused on the forests of Québec, a straightforward avenue to
investigate the generality of our findings is to compare our results with
regions that are experiencing different disturbance and climate regimes. A
natural candidate for such comparison is western Canada, which, over the last
decades, has suffered an unprecedented outbreak of the mountain pine beetle, a
severe drought (2001-2003) and extreme fire seasons [@michaelian_massive_2011;
@williamson_climate_2009]. If our results were to be confirmed, then we need to
rethink current forest management strategies as well as models of forest
responses to climate change in order to account for the synergistic effect of
disturbances.

\pagebreak


# References

<div id="refs"></div>

\pagebreak

## Data Accessibility Statement

The complete forest inventory dataset used in this study is available online at
https://www.donneesquebec.ca/recherche/fr/dataset/placettes-echantillons-permanentes-1970-a-aujourd-hui.
All code required to repeat the analyses will be made available online on
GitHub.

\pagebreak

# Tables

**Table 1**. Description of the predictors used in the multiple linear regression models.

|Variable name           |Variable description                                          |
|:-----------------------|:-----------------------------------------------------------------------------|
|**Baseline conditions** |                                                                               |
|Temp         |Mean temperature during growing season, 10-year average prior to first measurement (°C).    |
|Precip       |Total precipitation during growing season, 10-year average prior to first measurement (mm). |
|$\Delta$Time |Time interval between first and last measurements (years).                                  |
| **Climate change**         |                                                                             |
|$\Delta$Temp   |Slope between Temp and time (°C/y).                                                       |
|$\Delta$Precip |Slope between Temp and time (mm/y).                                                       |
|$\Delta$CMI    |Slope between Climate Moisture Index and time ($y^{-1}$).                                 |
|Temp min     |Extreme minimum temperature. Difference between minimum and mean temperature of the coldest period (°C).   |
|Temp min     |Extreme maximum temperature. Difference between maximum and mean temperature of the warmest period (°C).   |
|CMI min      |Extreme minimum Climate Moisture Index (CMI). Difference between minimum CMI and mean CMI (°C), as a proxy of drought.                    |
| **Disturbances**  |                                                                                         |
|Age          |Stand age (years)                                                                          |
|Harvest      |Tree harvesting. None (0), moderate (1) or major (2). Include clearcutting, selection cutting, shelterwood cutting, seed-tree cutting, etc.           |
|Fire         |Forest fires. None (0), moderate (1) or major (2).                                       |
|Outbreak     |Insect outbreaks. None (0), moderate (1) or major (2).           |
|Windfall     |Windfall or ice storm. None (0), moderate (1) or major (2).        |


\pagebreak

# Figures

![](ms/figures/fig1_region.png){width=5in}

**Figure 1.** Locations of the 6281 forest inventory plots in meridional Québec, Canada. Colors delimit the six bioclimatic domains. The two southernmost domains (orange) were combined in our analysis. The number of forest plots in each domain is written in parentheses.

\pagebreak

![](ms/figures/fig2_map_roll.png){width=6.6in}

**Figure 2.**
Maps of gains and losses in tree abundances (a) and latitudinal trends in
temporal ß diversity, decomposed into gains (blue) and losses (red) of boreal, pioneer and
temperate trees, for different levels of disturbance (b-d). The sizes and colors
of the points on the maps are proportional to the values of interest. The
latitudinal trends in temporal ß in a-d are based on moving averages computed on
each index against latitude (window size of 500 plots in panel a and 400 plots
in panels b-d), to smooth out local-scale fluctuations and highlight broad-scale
trends.


\pagebreak

![](ms/figures/fig3_hist.png){width=6.6in}

**Figure 3.**
Frequency distributions of temporal ß diversity in forests plots by bioclimatic
domains. Forests of different disturbance levels are stacked on top of each
other. The values written on the plots are the mean temporal ß diversity values followed by the
percentage of losses in parentheses.


\pagebreak

![](ms/figures/fig4_reg.png){width=6.6in}

**Figure 4.**

Slope coefficients from multiple regression models for (a) temporal ß diversity,
(b) species gains and (c) species losses and the corresponding variation
partitioning diagrams (d, e, f). Error bars represent one standard error of the
slope coefficient. For each model, only the selected predictors are shown, and
stars correspond to the level of significance of the p-value (* p < 0.05; ** p <
0.01; *** p < 0.001). See Table 1 for description of the predictor variables.

\pagebreak

![](ms/figures/fig5_thermo.png){width=6.6in}

**Figure 5.**

Thermophilization (i.e., change in community temperature index, $\Delta$CTI) and
successional process (i.e., change in community shade index, $\Delta$CSI) of
forests for different levels of disturbance. In the upper panels (a, b, c), the
latitudinal trends in $\Delta$CTI (black curve) and $\Delta$CSI (grey curve) are
based on moving averages computed on the indices against latitude (window size
of 400 plots). Positive values indicate an increase in warm-adapted species
(black) or in late-successional species (grey) over time. The dotted lines
represent the mean $\Delta$CTI (black) and $\Delta$CSI (grey) for different
levels of disturbance. In the lower panels (d, e, f), thermophilization of the
forest plots across the study area (All) and by bioclimatic domain. Temporal
shift of the mean (black line), left tail (red) and right tail (blue) of the
distribution of CTI; positive values indicate overall thermophilization,
increases of warm-adapted and decreases of cold-adapted species, respectively.

\pagebreak

![](ms/figures/fig6_spcontrib_cti.png){width=5.8in}

**Figure 6.**

Individual species contributions, through gains and losses, to thermophilization
of forest communities across the study area and for each bioclimatic domain. The
rectangles represent the mean contributions of given species through gains (dark
grey) or losses (light grey) across the study area, while the colored points
represent the mean contributions of given species through gains (solid) or
losses (empty) by domain. Letters next to species names correspond to
(T)emperate, (P)ioneer and (B)oreal species. Only species (28) that contributed
more than 0.005 in at least one domain are shown.


\pagebreak
