package com.github.andyglow.jsonschema.model

case class UserProfile(
  firstName: String,
  middleName: Option[String],
  lastName: String,
  age: Int,
  enabledFeatures: Set[BetaFeature] = Set(F0, F1),
  active: Active = On,
  credentials: Credentials = Credentials("anonymous", "-"),
  role: Role = Role.User,
  lastLoginMs: Option[Long], // = None; not supported currently for scala 2.11
  notes: Option[Notes] = Some(Notes("initial note", Nil)))
