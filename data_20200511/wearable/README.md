# Contents of your data export

Your data export contains the following files, which are described in further
detail below:

| File                                        | Description                                                                                     |
| ------------------------------------------- | ----------------------------------------------------------------------------------------------- |
| `anatomy-history.csv`                       | History of physiologically relevant data (e.g. height, weight) used for activity classification |
| `bodystates`                                | Directory of files containing general activity level over time                                  |
| `bodystates/bodystates-YYYY-MM.csv`         | General activity level for the given month                                                      |
| `day-metrics.csv`                           | Daily aggregates of various metrics                                                             |
| `events.csv`                                | Recorded activities (bike, manual, moderate activity, run, walk) and sleep                      |
| `habits-daily.csv`                          | Habits data written out day-by-day                                                              |
| `habits-weekly.csv`                         | Habits data written out week-by-week                                                            |
| `minute-metrics`                            | Directory of files containing per-minute sensor data                                            |
| `minute-metrics/minute-metrics-YYYY-MM.csv` | Per-minute sensor data for the given month                                                      |
| `phone-workouts.csv`                        | Workouts created on phone (playground feature)                                                  |
| `README.md`                                 | This file                                                                                       |
| `sleep.csv`                                 | Statistics about the times you were asleep with the watch on                                    |
| `sleep-stages.csv`                          | Your sleep, broken out into light/deep/REM, unknown, or interruption                            |
| `sleep-toss-and-turns.csv`                  | Times where you were tossing or turning in your sleep                                           |
| `time-zone-history.csv`                     | History of time zone changes                                                                    |



# General notes on the data format

## Dates and times

Timestamps are output both in
[UTC](https://en.wikipedia.org/wiki/Coordinated_Universal_Time) and in your
local time. The local time represents the time that you lived that actual
minute, accounting for any time zone changes you may have experienced. In
general, timestamps in UTC will be in columns ending in `(UTC)`, and timestamps
in your local time will be in columns ending in `(local)`. Timestamps are
formatted as `YYYY-MM-DD hh:mm:ss`.

Start times are _inclusive_ and end times are _exclusive_. For instance, if you
have a walk event that starts at `2016-03-04 12:01` and ends at
`2016-03-04 12:04`, the event and its metadata apply from the first instant of
`2016-03-04 12:01` to _just before_ the first instant of `2016-03-04 12:04`.

Note that your time zone may have changed while you wore the watch (if, for
example, you traveled); `time-zone-history.csv` contains a history of when you
were in each time zone.

For some data, a *day* is the most appropriate unit of time. In these cases,
the timestamp represents the entirety of the day as you lived it, and will be
formatted as `YYYY-MM-DD`.


## Calories

All values for calories represent what are referred to as calories in common
usage, but are also known as _large calories_, _kilogram calories_, or _food
calories_. See Wikipedia's
[article on the Calorie](https://en.wikipedia.org/wiki/Calorie) for more
details.



# Format of individual files:

## Anatomy History (`anatomy-history.csv`)

This file contains physiologically relevant data which is used in various
algorithms (e.g. calories burned) to process your data, and is tracked over
time.

| Field               | Description                                                 |
| ------------------- | ----------------------------------------------------------- |
| `Timestamp (UTC)`   | Data and time in UTC that this data became effective        |
| `Timestamp (local)` | Data and time in local time that this data became effective |
| `Weight`            | Your weight, in pounds                                      |
| `Height`            | Your height, in inches                                      |
| `Gender`            | `male` or `female`                                          |
| `Birthday`          | `YYYY-MM-DD`                                                |


## Bodystates (`bodystates/bodystates-YYYY-MM.csv`)

These files contain a history of general activity levels that we have
calculated from your data. Due to the large amount of data, these files are
grouped by month and stored in the `bodystates` subdirectory.

| Field                | Description                                                       |
| -------------------- | ----------------------------------------------------------------- |
| `Type`               | The activity level for this bodystate event; see below for values |
| `Start Time (UTC)`   | Date and time this event started, in UTC, inclusive               |
| `Start Time (local)` | Date and time this event started, in local time, inclusive        |
| `End time (UTC)`     | Date and time this event ended, in UTC, exclusive                 |
| `End time (local)`   | Date and time this event ended, in local time, exclusive          |
| `Calories`           | Calories burned during this event                                 |
| `Steps`              | Steps taken during this event                                     |
| `Average Heart Rate` | Average heart rate during the event                               |

Bodystate type values:

| Bodystate Type      | Description                                                                       |
| ------------------- | --------------------------------------------------------------------------------- |
| `off_wrist`         | Watch wasn't worn, though estimated calorie burn is still included                |
| `inactive`          | Little to no movement. Example: sitting or sleeping                               |
| `light_activity`    | Movement, but with low exertion. Example: light walking                           |
| `moderate_activity` | All higher activity levels. Same as the `moderate_activity` event in `events.csv` |


## Day Metrics (`day-metrics.csv`)

This file contains one row of summary data for every day for which data was
collected. Most of the data (e.g. activity data such as running and walking) is
aggregated over the period from midnight (inclusive) to midnight (exclusive).
However, there are two exceptions:

1. Sleep data is aggregated based on sleep events that begin after 5am on the
   given day and before 5am on the _following_ day. Note that the end of a
   sleep event may fall outside of this range and still be considered part of
   the given day.
2. Resting heart rate data is calculated from your prior night's sleep.
   Therefore it is aggregated from sleep events beginning after 5am of the
   _previous_ day to 5am of the given day.

Events that cross a border between two days are counted toward the day that
they _started_ on. For instance, if you took a walk from 11:45pm March 20 to
12:15am March 21 (e.g. crossing midnight), then the entire walk event would be
recorded on March 20.

"Interruptions", as referred to below, are small and exist as _part of_ a sleep
event, not between them. For instance, an "interruption" that happens at 5:30am
March 21, 2016 in a sleep that started at 1:00am the same morning will still
count toward March 20, 2016, because of when the containing sleep event
started.

The `bike`, `run` and `walk` related metrics do not include the time spent
during interruptions of those activities. Interruptions for an activity are
brief intervals between occurrences of the activity. For example if you walked
5 minutes, paused for 1 minute and then continued walking for 4 minutes the
walk activity would an interruption of 1 minute and metrics aggregated for the
other 9 minutes.

| Field                      | Description                                                                    |
| -------------------------- | ------------------------------------------------------------------------------ |
| `Date`                     | Date for the row of data. All values for this row are aggregates for this date |
| `Calories`                 | Calories burned                                                                |
| `Steps`                    | Steps taken                                                                    |
| `Sleep Duration`           | Minutes slept                                                                  |
| `Sleep Score`              | Sleep score (0-100), a measure of sleep quality for the day's associated night |
| `Number of Interruptions`  | Count of interruptions during sleep                                            |
| `Number of Toss and Turns` | Count of tosses and turns during sleep                                         |
| `Interruption Duration`    | Total time of all sleep interruptions                                          |
| `Resting Heart Rate`       | Resting heart rate (see above)                                                 |
| `Bike Calories`            | Calories burned while biking                                                   |
| `Bike Duration`            | Time spent biking                                                              |
| `Run Calories`             | Calories burned while running                                                  |
| `Run Duration`             | Time spent running                                                             |
| `Run Steps`                | Steps taken while running                                                      |
| `Walk Calories`            | Calories burned while walking                                                  |
| `Walk Duration`            | Time spent walking                                                             |
| `Walk Steps`               | Steps taken while walking                                                      |

## Device History (`device-history.csv`)

`device-history.csv` contains a history of the devices the user associated
their account with.

| Field                    | Description                                                            |
| ------------------------ | ---------------------------------------------------------------------- |
| Timestamp (UTC)          | The time in UTC that the user changed to this device                   |
| Timestamp (local)        | The time in the user's local timezone that they changed to this device |
| nth Device For This User | A number identifying the device within this file                       |
| Device Model             | The model of the device                                                |
| Firmware Version         | The version of the firmware on the device                              |

Note that `nth Device For This User` is **not** globally unique; the same
device will have the same number, but only within a single CSV file.


## Events (`events.csv`)

`events.csv` contains a history of Body IQ activities, moderate activities and
sleep that were recorded by your watch. Possible event types include `bike`,
`manual`, `moderate_activity`, `run`, `sleep` and `walk`.

`bike`, `run`, `walk`, are Body IQ activities which are detected and displayed
on the device.

`manual` activities are the workouts created on the watch.

`moderate_activity` is an interval when the watch detected that your
[metabolic equivalent (MET)](https://en.wikipedia.org/wiki/Metabolic_equivalent)
was greater than 3.3 and there were no other Body IQ activities that overlapped
during this event.

`sleep` is a Body IQ activity. For more details on your sleep refer to the
sleep related CSVs.

| Field                | Description                                                |
| -------------------- | ---------------------------------------------------------- |
| `Type`               | Event type; see below for values                           |
| `Start Time (UTC)`   | Date and time this event started, in UTC, inclusive        |
| `Start Time (local)` | Date and time this event started, in local time, inclusive |
| `End Time (UTC)`     | Date and time this event ended, in UTC, exclusive          |
| `End Time (local)`   | Date and time this event ended, in local time, exclusive   |
| `Calories`           | Calories burned during this event                          |
| `Steps`              | Steps taken during this event                              |
| `Duration`           | Duration of this event in minutes, excluding interruptions |
| `Average Heart Rate` | Average heart rate during the event                        |

Event type values:

| Event Type              | Description                                       |
| ----------------------- | ------------------------------------------------- |
| `bike`                  | Biking                                            |
| `manual`                | Workout event created manually from the watch     |
| `moderate_activity`     | Unclassified event of moderate or higher activity |
| `run`                   | Running                                           |
| `sleep`                 | Sleeping                                          |
| `walk`                  | Walking                                           |


## Habits (`habits-weekly.csv`)

This file contains weekly results of your habits, allowing you to see patterns
based on days of the week.

| Field            | Description                                                     |
| ---------------- | --------------------------------------------------------------- |
| `Habit`          | Name of the habit                                               |
| `Week`           | The date for Monday of the week on which this data was recorded |
| `Level`          | Level of habit accomplishment (0-4)                             |
| `Weekly Goal`    | Goal number of days for this week                               |
| `Weekly Success` | `true` or `false`                                               |
| `Daily Target`   | Goal for each day                                               |
| `Mon`            | Result for Monday                                               |
| `Tue`            | Result for Tuesday                                              |
| `Wed`            | Result for Wednesday                                            |
| `Thu`            | Result for Thursday                                             |
| `Fri`            | Result for Friday                                               |
| `Sat`            | Result for Saturday                                             |
| `Sun`            | Result for Sunday                                               |


## Habits (`habits-daily.csv`)

This file contains daily results of your habits, allowing you to evaluate
long-term trends in your habit performance.

| Field                   | Description                                       |
| ----------------------- | ------------------------------------------------- |
| `Habit`                 | Name of the habit                                 |
| `Date`                  | The date for which this data was recorded         |
| `Level`                 | Level of habit accomplishment (1-5)               |
| `Weekly Goal`           | Goal number of days for this week                 |
| `Weekly Success`        | `true` or `false`                                 |
| `Daily Target`          | Goal for the day                                  |
| `Result`                | Result for the day                                |
| `Daily Success`         | `true` or `false`                                 |


## Minute Metrics (`minute-metrics/minute-metrics-YYYY-MM.csv`)

These files contain one row of sensor data for every minute that data was
recorded. Due to the large amount of data, these files are grouped by month and
stored in the `minute-metrics` subdirectory.

| Field                     | Description                                        |
| ------------------------- | ---------------------------------------------------|
| `Timestamp (UTC)`         | The minute in UTC                                  |
| `Timestamp (local)`       | The minute in local time                           |
| `Calories`                | Calories burned                                    |
| `GSR`                     | Average Galvanic Skin Response (GSR)               |
| `Heart Rate`              | Average heart rate                                 |
| `Skin Temperature (F)`    | Average skin temperature, in degrees Fahrenheit    |
| `Steps`                   | Steps taken                                        |


## Phone Workouts (`phone-workouts.csv`)

This file contains workouts that you manually created via our mobile app.

_Note_: Workouts that you manually created on the watch are recorded as events
of type "manual" in the `events.csv` file.

They are either:

* Standalone workouts: these are not strictly tied to other events, but events
  recorded elsewhere that overlap this workout may have been hidden in the user
  interface.
* Edits to existing events: when an event is edited in the mobile app, it is
  recorded as a phone workout. It can also have an adjusted start/end time.

| Field                | Description                                                  |
| -------------------- | ------------------------------------------------------------ |
| `Start Time (UTC)`   | Date and time this workout started, in UTC, inclusive        |
| `Start Time (local)` | Date and time this workout started, in local time, inclusive |
| `End Time (UTC)`     | Date and time this workout ended, in UTC, exclusive          |
| `End Time (local)`   | Date and time this workout ended, in local time, exclusive   |
| `Type`               | Type of workout, as entered by user                          |
| `Name`               | Name of workout, as entered by user                          |
| `Description`        | Description of workout, as entered by user                   |


## Sleep

Sleep data is split into three files, described below.

### Sleep Overview (`sleep.csv`)

This file contains a high-level overview of the sleep events that we
automatically detected for you.

| Field                    | Description                                                                                                        |
| ------------------------ | ------------------------------------------------------------------------------------------------------------------ |
| `Start Time (UTC)`       | Date and time this sleep started, in UTC, inclusive                                                                |
| `Start Time (local)`     | Date and time this sleep started, in local time, inclusive                                                         |
| `End Time (UTC)`         | Date and time this sleep ended, in UTC, exclusive                                                                  |
| `End Time (local)`       | Date and time this sleep ended, in local time, exclusive                                                           |
| `Duration`               | Time you were actually asleep, in minutes; excludes time that sleep was "interrupted"                              |
| `Light (minutes)`        | Time you were in light sleep, in minutes                                                                           |
| `Deep (minutes)`         | Time you were in deep sleep, in minutes                                                                            |
| `REM (minutes)`          | Time you were in REM sleep, in minutes                                                                             |
| `Interruption (minutes)` | Time your sleep was interrupted, in minutes                                                                        |
| `Unknown (minutes)`      | Time your sleep stage was unknown, in minutes                                                                      |
| `Tosses and Turns`       | Count of "tosses and turns" you experienced                                                                        |

### Tosses and Turns (`sleep-toss-and-turns.csv`)

This file contains detailed information about timing of all tosses and turns
that the watch detected while you were asleep. The sleep start and end times
are also included to allow easy reference to the sleep event in question.

| Field                      | Description                                                 |
| -------------------------- | ----------------------------------------------------------- |
| `Sleep Start Time (UTC)`   | Date and time this sleep started, in UTC, inclusive         |
| `Sleep Start Time (local)` | Date and time this sleep started, in local time, inclusive  |
| `Sleep End Time (UTC)`     | Date and time this sleep ended, in UTC, exclusive           |
| `Sleep End Time (local)`   | Date and time this sleep ended, in local time, exclusive    |
| `Timestamp (UTC)`          | Data and time in UTC that this toss or turn occurred        |
| `Timestamp (local)`        | Data and time in local time that this toss or turn occurred |


### Sleep Stages (`sleep-stages.csv`)

This file contains information about the stages of your sleep, including when
you were in light, deep, and REM sleep. The sleep start and end times are also
included to allow easy reference to the sleep event in question.

| Field                      | Description                                                      |
| -------------------------- | ---------------------------------------------------------------- |
| `Sleep Start Time (UTC)`   | Date and time this sleep started, in UTC, inclusive              |
| `Sleep Start Time (local)` | Date and time this sleep started, in local time, inclusive       |
| `Sleep End Time (UTC)`     | Date and time this sleep ended, in UTC, exclusive                |
| `Sleep End Time (local)`   | Date and time this sleep ended, in local time, exclusive         |
| `Start Time (UTC)`         | Date and time this sleep stage started, in UTC, inclusive        |
| `Start Time (local)`       | Date and time this sleep stage started, in local time, inclusive |
| `End Time (UTC)`           | Date and time this sleep stage ended, in UTC, exclusive          |
| `End Time (local)`         | Date and time this sleep stage ended, in local time, exclusive   |
| `Stage`                    | Type of sleep stage; see below for values                        |

| Sleep Stages            | Description                                       |
| ----------------------- | ------------------------------------------------- |
| `deep sleep`            | Deep sleep                                        |
| `interruption`          | Interruption (i.e. brief awakening)               |
| `light sleep`           | Light sleep                                       |
| `REM sleep`             | REM sleep                                         |
| `unknown`               | Unable to classify your sleep during this time    |


## Time Zone History (`time-zone-history.csv`)

This file contains the history of all time zone changes that were recorded as
you wore the watch and used our application(s). These time zone changes are
used to allocate sensor data and events to the appropriate day - as you lived
it.

| Field                 | Description                                         |
| --------------------- | --------------------------------------------------- |
| `Timestamp (UTC)`     | Date and time when the user entered a new time zone |
| `Time Zone`           | Time zone that was entered                          |
