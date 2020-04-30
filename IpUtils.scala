import java.math.BigInteger
import java.net.InetAddress

/** Created by Gu Chao on 10/11/2019. */
object IpUtils {

  /** 0.0.0.0 - 255.255.255.255 */
  private[this] val ipv4Regex = """^(([1-9]?[0-9]|1[0-9]?[0-9]|2[0-4][0-9]|25[0-5])\.){3}([1-9]?[0-9]|1?[0-9]?[0-9]|2[0-4][0-9]|25[0-5])$""".r

  /** ::0/::0000/0000:0000:0000:0000:0000:0000:0000:0000 - ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff
   *  https://baike.baidu.com/item/IPv6/172297?fr=aladdin
   *  [文件回车符为"\n"]
   */
  private[this] val ipv6Regex =
    """^(([0-9A-Fa-f]{1,4}:){7}[0-9A-Fa-f]{1,4})|
      #(([0-9A-Fa-f]{1,4}:){6}:[0-9A-Fa-f]{1,4})|
      #(([0-9A-Fa-f]{1,4}:){5}(:[0-9A-Fa-f]{1,4}){1,2})|
      #(([0-9A-Fa-f]{1,4}:){4}(:[0-9A-Fa-f]{1,4}){1,3})|
      #(([0-9A-Fa-f]{1,4}:){3}(:[0-9A-Fa-f]{1,4}){1,4})|
      #(([0-9A-Fa-f]{1,4}:){2}(:[0-9A-Fa-f]{1,4}){1,5})|
      #(([0-9A-Fa-f]{1,4}:){1}(:[0-9A-Fa-f]{1,4}){1,6})|
      #(([0-9A-Fa-f]{1,4}:){1,7}:)|
      #(:(:[0-9A-Fa-f]{1,4}){1,7})|
      #(::)|
      #(([0-9A-Fa-f]{1,4}:){6}((0?[0-9]{1,2}|1[0-9]{2}|2[0-4][0-9]|25[0-5])\.){3}(0?[0-9]{1,2}|1[0-9]{2}|2[0-4][0-9]|25[0-5]))|
      #(([0-9A-Fa-f]{1,4}:){5}:((0?[0-9]{1,2}|1[0-9]{2}|2[0-4][0-9]|25[0-5])\.){3}(0?[0-9]{1,2}|1[0-9]{2}|2[0-4][0-9]|25[0-5]))|
      #(([0-9A-Fa-f]{1,4}:){4}:((0?[0-9]{1,2}|1[0-9]{2}|2[0-4][0-9]|25[0-5])\.){3}(0?[0-9]{1,2}|1[0-9]{2}|2[0-4][0-9]|25[0-5]))|
      #(([0-9A-Fa-f]{1,4}:){3}:((0?[0-9]{1,2}|1[0-9]{2}|2[0-4][0-9]|25[0-5])\.){3}(0?[0-9]{1,2}|1[0-9]{2}|2[0-4][0-9]|25[0-5]))|
      #(([0-9A-Fa-f]{1,4}:){2}:((0?[0-9]{1,2}|1[0-9]{2}|2[0-4][0-9]|25[0-5])\.){3}(0?[0-9]{1,2}|1[0-9]{2}|2[0-4][0-9]|25[0-5]))|
      #(([0-9A-Fa-f]{1,4}:){1}:((0?[0-9]{1,2}|1[0-9]{2}|2[0-4][0-9]|25[0-5])\.){3}(0?[0-9]{1,2}|1[0-9]{2}|2[0-4][0-9]|25[0-5]))|
      #(:(:[0-9A-Fa-f]{1,4}){0,5}:((0?[0-9]{1,2}|1[0-9]{2}|2[0-4][0-9]|25[0-5])\.){3}(0?[0-9]{1,2}|1[0-9]{2}|2[0-4][0-9]|25[0-5]))$
    """.stripMargin('#').replaceAll("\n", "").r

  /* ipv4 convert from string to long */
  def ipv4FromStringToLong(ip: Option[String]): Option[Long] = {
    if (ip.isDefined) {
      val opt = ipv4Regex.findFirstIn(ip.get)
      if (opt.isDefined) {
        val ips = ip.get.split("\\.", -1)
        Some((ips(0).toLong << 24) + (ips(1).toLong << 16) + (ips(2).toLong << 8) + ips(3).toLong)
      } else None
    } else None
  }

  /** ipv6 convert from string to long
   *  https://docs.oracle.com/javase/8/docs/api/java/net/Inet6Address.html#scoped
   */
  def ipv6FromStringToLong(ip: Option[String]): Option[BigInteger] = {
    if (ip.isDefined) {
      val opt = ipv6Regex.findFirstIn(ip.get)
      if (opt.isDefined) {
        val inetAddress = InetAddress.getByName(opt.get)
        val bytes = inetAddress.getAddress
        Some(new BigInteger(1, bytes))
      } else None
    } else None
  }

  /* ipv4 convert from long to string */
  def ipv4FromLongToString(ip: Long): String =
    if (ip >= 0 && ip <= 4294967295L) Array(ip >>> 24, (ip & 0x00FFFFFF) >>> 16, (ip & 0x0000FFFF) >>> 8, ip & 0x000000FF).mkString(".") else Constants.LABEL_EMPTY

  /* ipv6 convert from long to String */
  def ipv6FromLongToString(ip: Option[BigInteger]): Option[String] = {
    if (ip.isDefined) {
      Some(String.format("%s:%s:%s:%s:%s:%s:%s:%s",
        Integer.toHexString(ip.get.shiftRight(112).and(BigInteger.valueOf(0xFFFF)).intValue).toUpperCase,
        Integer.toHexString(ip.get.shiftRight(96).and(BigInteger.valueOf(0xFFFF)).intValue).toUpperCase,
        Integer.toHexString(ip.get.shiftRight(80).and(BigInteger.valueOf(0xFFFF)).intValue).toUpperCase,
        Integer.toHexString(ip.get.shiftRight(64).and(BigInteger.valueOf(0xFFFF)).intValue).toUpperCase,
        Integer.toHexString(ip.get.shiftRight(48).and(BigInteger.valueOf(0xFFFF)).intValue).toUpperCase,
        Integer.toHexString(ip.get.shiftRight(32).and(BigInteger.valueOf(0xFFFF)).intValue).toUpperCase,
        Integer.toHexString(ip.get.shiftRight(16).and(BigInteger.valueOf(0xFFFF)).intValue).toUpperCase,
        Integer.toHexString(ip.get.and(BigInteger.valueOf(0xFFFF)).intValue)).toUpperCase)
    } else Some(Constants.LABEL_EMPTY)
  }

  // whether is legal v6 or not
  def isLegalIPV6(ip: Option[String]): Boolean = {
    val opt = ipv6Regex.findFirstIn(ip.get)
    if (opt.isDefined) true else false
  }

  /* whether is a private ip or not */
  def isPrivateIP(ip: Long): Int = ip match {
    case i if i >=  167772160L && i <=  184549375L => Constants.LABEL_ONE // 10.0.0.0 - 10.255.255.255
    case i if i >= 1681915904L && i <= 1686110207L => Constants.LABEL_ONE // 100.64.0.0 - 100.127.255.255
    case i if i >= 2214592512L && i <= 2231369727L => Constants.LABEL_ONE // 132.0.0.0 - 132.255.255.255
    case i if i >= 2886729728L && i <= 2887778303L => Constants.LABEL_ONE // 172.16.0.0 - 172.31.255.255
    case i if i >= 3232235520L && i <= 3232301055L => Constants.LABEL_ONE // 192.168.0.0 - 192.168.255.255
    case _ => Constants.LABEL_ZERO
  }

  /** get ipv4 number */
  def getIpNum(qsip: Option[String], zzip: Option[String]): Option[Long] = {
    if (qsip.isDefined && zzip.isDefined) {
      val qsipSzh = ipv4FromStringToLong(qsip)
      val zzipSzh = ipv4FromStringToLong(zzip)
      if (qsipSzh.isDefined && zzipSzh.isDefined) {
        Some(zzipSzh.get - qsipSzh.get + 1)
      } else None
    } else None
  }

  /** get ipv6 number */
  def getIpv6Num(qsip: Option[String], zzip: Option[String]): Option[BigInteger] = {
    if (qsip.isDefined && zzip.isDefined) {
      val qsipSzh = ipv6FromStringToLong(qsip)
      val zzipSzh = ipv6FromStringToLong(zzip)
      if (qsipSzh.isDefined && zzipSzh.isDefined) {
        Some(zzipSzh.get.subtract(qsipSzh.get).add(new BigInteger("1")))
      } else None
    } else None
  }

  def main(args: Array[String]): Unit = {
    println(ipv6FromStringToLong(Some("FFFF:FFFF:FFFF:FFFF:FFFF:FFFF:FFFF:FFFF")))
    println(ipv6FromStringToLong(Some("ABCD:EF01:2345:6789:ABCD:EF01:2345:6789")))
    println(ipv6FromStringToLong(Some("2001:0DB8:0000:0023:0008:0800:200C:417A")))
    println(ipv6FromStringToLong(Some("2001:DB8:0:23:8:800:200C:417A")))
    println(ipv6FromStringToLong(Some("FF01:0:0:0:0:0:0:1101")))
    println(ipv6FromStringToLong(Some("FF01::1101")))
    println(ipv6FromStringToLong(Some("0:0:0:0:0:0:0:1")))
    println(ipv6FromStringToLong(Some("::1")))
    println(ipv6FromStringToLong(Some("0:0:0:0:0:0:0:0")))
    println(ipv6FromStringToLong(Some("::")))
    println(ipv6FromStringToLong(Some("::192.168.0.1")))
    println(ipv6FromStringToLong(Some("::FFFF:192.168.0.1")))

    println(isLegalIPV6(Some("ABCD:EF01:2345:6789:ABCD:EF01:2345:6789")))
    println(isLegalIPV6(Some("2001:0DB8:0000:0023:0008:0800:200C:417A")))
    println(isLegalIPV6(Some("2001:DB8:0:23:8:800:200C:417A")))
    println(isLegalIPV6(Some("FF01:0:0:0:0:0:0:1101")))
    println(isLegalIPV6(Some("FF01::1101")))
    println(isLegalIPV6(Some("0:0:0:0:0:0:0:1")))
    println(isLegalIPV6(Some("::1")))
    println(isLegalIPV6(Some("0:0:0:0:0:0:0:0")))
    println(isLegalIPV6(Some("::")))
    println(isLegalIPV6(Some("::192.168.0.1")))
    println(isLegalIPV6(Some("::FFFF:192.168.0.1")))

    println(ipv6FromStringToLong(Some("2409:8020::")))
    println(ipv6FromStringToLong(Some("2409:8127:FFFF:FFFF:FFFF:FFFF:FFFF:FFFF")))
    println(getIpv6Num(Some("2409:8127::"), Some("2409:8127:FFFF:FFFF:FFFF:FFFF:FFFF:FFFF")))

    println(ipv6FromLongToString(Some(new BigInteger("42540488182159607633435240198452018586"))))
    println(ipv6FromLongToString(Some(new BigInteger("42540766411282593502542288127932514682"))))
    println(ipv6FromLongToString(Some(new BigInteger("228367255721259569362527394270995113865"))))
  }

}
