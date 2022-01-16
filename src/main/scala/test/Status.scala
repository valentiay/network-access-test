package test

enum Status {
  case ok
  case timeout
  case unreachable
  case dns_err
  case other_err(cause: Throwable)

  case http_ok(code: Int)
  case http_err(code: Int)
}
