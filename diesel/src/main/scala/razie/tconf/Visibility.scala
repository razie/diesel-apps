/**   ____    __    ____  ____  ____,,___     ____  __  __  ____
  *  (  _ \  /__\  (_   )(_  _)( ___)/ __)   (  _ \(  )(  )(  _ \           Read
  *   )   / /(__)\  / /_  _)(_  )__) \__ \    )___/ )(__)(  ) _ <     README.txt
  *  (_)\_)(__)(__)(____)(____)(____)(___/   (__)  (______)(____/    LICENSE.txt
  **/
package razie.tconf

/** visibility settings of specifications */
object Visibility {
  /** public - anyone can see it, no login required */
  final val PUBLIC = "Public"
  /** only the owner can see */
  final val PRIVATE = "Private"
  /** all club/group members can see it */
  final val CLUB = "Club"
  /** only club admins can see - i.e. registration forms */
  final val CLUB_ADMIN = "ClubAdmin"
  /** any coach from your clubs can see it */
  final val CLUB_COACH = "ClubCoach"

  /** member of website */
  final val MEMBER = "Member"
  final val BASIC = "Basic"
  final val GOLD = "Gold"
  final val PLATINUM = "Platinum"
  final val UNOBTANIUM = "Unobtanium"
  final val MODERATOR = "Moderator"
}
