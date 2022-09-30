const MONTHS = [
  {
    short: "Jan",
    long: "January",
  },
  {
    short: "Feb",
    long: "February",
  },
  {
    short: "Mar",
    long: "March",
  },
  {
    short: "Apr",
    long: "April",
  },
  {
    short: "May",
    long: "May",
  },
  {
    short: "Jun",
    long: "June",
  },
  {
    short: "Jul",
    long: "July",
  },
  {
    short: "Aug",
    long: "August",
  },
  {
    short: "Sep",
    long: "September",
  },
  {
    short: "Oct",
    long: "October",
  },
  {
    short: "Nov",
    long: "November",
  },
  {
    short: "Dec",
    long: "December",
  },
];

const DAYS = [
  {
    short: "Sun",
    long: "Sunday",
  },
  {
    short: "Mon",
    long: "Monday",
  },
  {
    short: "Tue",
    long: "Tuesday",
  },
  {
    short: "Wed",
    long: "Wednesday",
  },
  {
    short: "Thu",
    long: "Thursday",
  },
  {
    short: "Fri",
    long: "Friday",
  },
  {
    short: "Sat",
    long: "Saturday",
  },
];

const getDateEnding = (num) => {
  const stEnding = [1, 21, 31];
  const ndEnding = [2, 22];
  const rdEnding = [3, 23];
  if (stEnding.includes(num)) return "st";
  if (ndEnding.includes(num)) return "nd";
  if (rdEnding.includes(num)) return "rd";
  return "th";
};

export default {
  install: (app) => {
    app.config.globalProperties.$getTimeComponents = (ts) => {
      const date = new Date(ts);
      return {
        year: date.getFullYear(),
        month: date.getMonth() + 1,
        shortMonth: MONTHS[date.getMonth()].short,
        longMonth: MONTHS[date.getMonth()].long,
        date: date.getDate(),
        dateEnding: getDateEnding(date.getDate()),
        longDate: date.getDate() + getDateEnding(date.getDate()),
        day: date.getDay(),
        hour: (date.getHours() + 12) % 12,
        meridian: date.getHours() < 12 ? "am" : "pm",
        minutes: date.getMinutes(),
        shortDay: DAYS[date.getDay()].short,
        longDay: DAYS[date.getDay()].long,
      };
    };
    app.config.globalProperties.$getMonthName = (num, long = true) => {
      return MONTHS[num][long ? "long" : "short"];
    };
    app.config.globalProperties.$buildDate = function (ts, string) {
      const comps = this.$getTimeComponents(ts);
      Object.keys(comps).forEach((key) => {
        string = string.replace(`{${key}}`, comps[key]);
      });
      return string;
    };
  },
};
