/**
 * ____    __    ____  ____  ____,,___     ____  __  __  ____
 * (  _ \  /__\  (_   )(_  _)( ___)/ __)   (  _ \(  )(  )(  _ \           Read
 * )   / /(__)\  / /_  _)(_  )__) \__ \    )___/ )(__)(  ) _ <     README.txt
 * (_)\_)(__)(__)(____)(____)(____)(___/   (__)  (______)(____/    LICENSE.txt
 */
package razie.wiki.model

/** visibility settings of topics */
object Visibility {
  /** public - anyone can see it, no login required */
  final val PUBLIC = "Public"
  /** only the owner can see */
  final val PRIVATE = "Private"
  /** all club/group members can see it */
  final val CLUB = "Club"
  /** only club admins can see - i.e. registration forms */
  final val CLUB_ADMIN = "ClubAdmin"

  /** member of website */
  final val MEMBER = "Member"
  /** member of website */
  final val BASIC = "Basic"
  /** member of website */
  final val GOLD = "Gold"
  /** member of website */
  final val PLATINUM = "Platinum"
}
