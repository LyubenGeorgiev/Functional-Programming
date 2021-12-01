#include <iostream>
#include <iomanip>


unsigned getWeight(unsigned date)
{
    unsigned revCopy = date % 10;
    date /= 10;
    unsigned result = 0;

    while (date)
    {
        revCopy *= 10;
        revCopy += date % 10;
        date /= 10; 
    }
    
    while (revCopy)
    {
        unsigned multiplier = revCopy % 10;
        revCopy /= 10;
        unsigned copyOfRevCopy = revCopy;
        
        while (copyOfRevCopy)
        {
            result += multiplier * (copyOfRevCopy % 10);
            copyOfRevCopy /= 10;
        }
    }

    return result;
}

void getMagicDates(unsigned yearA, unsigned yearB, unsigned weight)
{
    unsigned days[12] = {31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30,31};
    bool found = false;

    for (unsigned curYear = yearA; curYear <= yearB; ++curYear)
    {
        if (((curYear % 4 == 0) && (curYear % 100 != 0)) || (curYear % 400 == 0))
        {
            days[1] = 29;
        }
        else
        {
            days[1] = 28;
        }

        for (unsigned curMonth = 1; curMonth <= 12; ++curMonth)
        {
            for (unsigned curDay = 1; curDay <= days[curMonth - 1]; ++ curDay)
            {
                unsigned curDate = curDay;
                unsigned curMonthCopy = curMonth;
                unsigned curYearCopy = curYear;

                while (curMonthCopy)
                {
                    curDate *= 10;
                    curDate += curMonthCopy % 10;
                    curMonthCopy /= 10;
                }

                while (curYearCopy)
                {
                    curDate *= 10;
                    curDate += curYearCopy % 10;
                    curYearCopy /= 10;
                }

                if (getWeight(curDate) == weight)
                {
                    found = true;
                    std::cout << std::setfill('0') << std::setw(2) << curDay << '-' 
                              << std::setfill('0') << std::setw(2) << curMonth << '-' << curYear << '\n';
                }
            }
        }
    }
    if (!found)
    {
        std::cout << "No";
    }
}

int main()
{
    unsigned yearA, yearB, weight;
    std::cin >> yearA >> yearB >> weight;

    getMagicDates(yearA, yearB, weight);

    return 0;
}