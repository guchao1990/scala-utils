import java.text.{DateFormat, ParseException, SimpleDateFormat}
import java.util.{Calendar, Date}

/** Created by Gu Chao on 10/11/2019. */
object DateUtils {

  private[this] val DF_YYYY_MM_DD_HH_MM_SS = new ThreadLocal[DateFormat] { 
    override def initialValue: DateFormat = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss") 
  }
  private[this] val DF_YYYY_MM_DD = new ThreadLocal[DateFormat] { 
    override def initialValue: DateFormat = new SimpleDateFormat("yyyy-MM-dd") 
  }
  private[this] val DF_YYYYMMDD = new ThreadLocal[DateFormat] {
    override def initialValue: DateFormat = new SimpleDateFormat("yyyyMMdd")
  }
  private[this] val DF_YYYYMMDDHHMMSS = new ThreadLocal[DateFormat] {
    override def initialValue: DateFormat = new SimpleDateFormat("yyyyMMddHHmmss")
  }

  /** Date -> String("yyyy-MM-dd HH:mm:ss") */
  def getDateTimeByNowDate: String = DF_YYYY_MM_DD_HH_MM_SS.get.format(new Date)

  /** Date -> String("yyyyMMddHHmmss") */
  def getDateTime2ByNowDate: String = DF_YYYYMMDDHHMMSS.get.format(new Date)

  /** Date -> String("yyyy-MM-dd") */
  def getDateByNowDate: String = DF_YYYY_MM_DD.get.format(new Date)

  /** Date -> String("yyyyMMdd") */
  def getDate2ByNowDate: String = DF_YYYYMMDD.get.format(new Date)

  /** Date -> String("yyyy-MM-dd") */
  def getSignDateByDate(day: Int): String = {
    val c = Calendar.getInstance
    c.setTime(new Date)
    c.add(Calendar.DATE, day)
    DF_YYYY_MM_DD.get.format(c.getTime)
  }

  /** Date -> String("yyyyMMdd") */
  def getSignDate2ByDate(day: Int): String = {
    val c = Calendar.getInstance
    c.setTime(new Date)
    c.add(Calendar.DATE, day)
    DF_YYYYMMDD.get.format(c.getTime)
  }

  /** Long -> String("yyyy-MM-dd HH:mm:ss") */
  def getDateByStringLongDate(d: String, isMillisecond: Boolean = true): String = try {
    val date = if (isMillisecond) new Date(d.toLong) else new Date(d.toLong * 1000)
    DF_YYYY_MM_DD_HH_MM_SS.get.format(date)
  } catch {
    case _: NumberFormatException => ""
  }

  /** Long -> String("yyyy-MM-dd HH:mm:ss") */
  def getDateByLongDate(d: Long, isMillisecond: Boolean = true): String = {
    val date = if (isMillisecond) new Date(d) else new Date(d * 1000)
    DF_YYYY_MM_DD_HH_MM_SS.get.format(date)
  }

  /** String("yyyy-MM-dd HH:mm:ss") -> Long */
  def getDateBySignDate(d: Option[String]): Option[Long] = {
    if (d.isDefined) {
      try {
        Some(DF_YYYY_MM_DD_HH_MM_SS.get.parse(d.get).getTime)
      } catch {
        case _: ParseException => None
      }
    } else None
  }

}
