package it.unibo.pps.ex2

import scala.collection.*

trait ConferenceReviewing:

  enum Question:
    case Relevance
    case Significance
    case Confidence
    case Final

  def loadReview(article: Int, scores: Map[Question, Int]): Unit
  def loadReview(article: Int, relevance: Int, significance: Int, confidence: Int, fin: Int): Unit
  def orderedScores(article: Int, question: Question): List[Int]
  def averageFinalScore(article: Int): Double
  def acceptedArticles(): Set[Int]
  def sortedAcceptedArticles(): List[Pair[Int, Double]]
  def averageWeightedFinalScoreMap(): Map[Int, Double]

class ConferenceReviewingImpl extends ConferenceReviewing:
  private var reviews: List[Pair[Int, Map[Question, Int]]] = List()

  override def loadReview(article: Int, scores: Map[Question, Int]): Unit =
    require(scores.size == Question.values.length)
    reviews = reviews.+:(Pair(article, scores))

  override def loadReview(article: Int, relevance: Int, significance: Int, confidence: Int, fin: Int): Unit =
    val map: Map[Question, Int] = Map(
      Question.Relevance -> relevance,
      Question.Significance -> significance,
      Question.Confidence -> confidence,
      Question.Final -> fin)
    reviews = reviews.+:(Pair(article, map))

  override def orderedScores(article: Int, question: Question): List[Int] =
    reviews
      .filter(p => p.getX == article)
      .flatMap(_.getY.get(question))
      .sorted

  override def averageFinalScore(article: Int): Double =
    val averageScore = reviews
      .filter(_.getX == article)
      .map(_.getY.getOrElse(Question.Final, 0).toDouble)
    averageScore.sum / averageScore.length

  private def accepted(article: Int): Boolean =
    averageFinalScore(article) > 5 &&
      reviews
        .filter(p => p.getX == article)
        .exists(p => p.getY.getOrElse(Question.Relevance, 0) >= 8)

  override def acceptedArticles(): Set[Int] =
    reviews
      .map(p => p.getX)
      .distinct
      .filter(p => accepted(p))
      .toSet

  override def sortedAcceptedArticles(): List[Pair[Int, Double]] =
    acceptedArticles()
      .map(e => Pair(e, averageFinalScore(e)))
      .toList
      .sortBy(p => p.getY)

  private def averageWeightedFinalScore(article: Int): Double =
    val averageScore = reviews
      .filter(p => p.getX == article)
      .map(p => p.getY.getOrElse(Question.Final, 0) * p.getY.getOrElse(Question.Confidence, 0) / 10.0)
    averageScore.sum / averageScore.length

  override def averageWeightedFinalScoreMap(): Map[Int, Double] =
    reviews
      .map(p => p.getX)
      .distinct
      .map(p => p -> averageWeightedFinalScore(p))
      .toMap
