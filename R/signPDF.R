# signPDF.R
# Digital signature for a (report) PDF file, as an alternative to the
# print-sign-scan workflow. Produces a DETACHED cryptographic signature: the PDF
# bytes are signed with the signer's private key (RSA/EC) using SHA-256, and the
# signature plus metadata are written to a sidecar file '<pdf>.sig'. Anyone can
# later verify, with the signer's public key/certificate, that the PDF is intact
# and was signed by that key (tamper-evidence + non-repudiation). This needs the
# 'openssl' package (Suggests). Note: this is a detached signature, not a PAdES
# signature embedded inside the PDF (embedding requires a dedicated PDF tool).

signPDF = function(pdf, key, password=NULL, signer="", role="",
                   sigFile=paste0(pdf, ".sig"), writePubkey=TRUE)
{
  if (!requireNamespace("openssl", quietly=TRUE))
    stop("Package 'openssl' is required for digital signing. install.packages(\"openssl\").")
  if (!file.exists(pdf)) stop("PDF not found: ", pdf)

  k = if (inherits(key, "key")) key else openssl::read_key(key, password=password)
  data = readBin(pdf, "raw", file.info(pdf)$size)
  sha = as.character(openssl::sha256(data))
  sig = openssl::signature_create(data, hash = openssl::sha256, key = k)
  pubpem = as.character(openssl::write_pem(k$pubkey))
  pubfp = as.character(openssl::sha256(charToRaw(pubpem)))

  out = c("-----BEGIN PDF DIGITAL SIGNATURE-----",
          paste0("file: ", basename(pdf)),
          paste0("sha256: ", sha),
          paste0("signer: ", if (nzchar(signer)) signer else Sys.info()[["login"]]),
          paste0("role: ", role),
          "algorithm: RSA/EC-SHA256 (openssl::signature_create)",
          paste0("datetime: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), " ", Sys.timezone()),
          paste0("pubkey_sha256: ", pubfp),
          "signature_base64:",
          strwrap(openssl::base64_encode(sig), width=64),
          "-----END PDF DIGITAL SIGNATURE-----")
  writeLines(out, sigFile)
  if (isTRUE(writePubkey)) writeLines(pubpem, paste0(pdf, ".pubkey.pem"))

  cat(sprintf("Digitally signed '%s'\n  signature : %s\n%s",
              pdf, sigFile,
              if (isTRUE(writePubkey)) sprintf("  public key: %s\n", paste0(pdf, ".pubkey.pem")) else ""))
  invisible(list(sigFile=sigFile, sha256=sha, signer=signer, pubkey_sha256=pubfp))
}

verifyPDF = function(pdf, sigFile=paste0(pdf, ".sig"), pubkey=paste0(pdf, ".pubkey.pem"))
{
  if (!requireNamespace("openssl", quietly=TRUE))
    stop("Package 'openssl' is required for verification. install.packages(\"openssl\").")
  if (!file.exists(pdf))     stop("PDF not found: ", pdf)
  if (!file.exists(sigFile)) stop("signature file not found: ", sigFile)

  L = readLines(sigFile)
  getf = function(tag) {
    i = grep(paste0("^", tag, ": "), L)
    if (length(i) == 0) "" else sub(paste0("^", tag, ": "), "", L[i[1]])
  }
  i0 = grep("^signature_base64:", L); i1 = grep("^-----END", L)
  sig = openssl::base64_decode(paste(L[(i0 + 1):(i1 - 1)], collapse=""))

  pub = if (inherits(pubkey, c("pubkey", "cert"))) {
          if (inherits(pubkey, "cert")) pubkey$pubkey else pubkey
        } else {
          txt = paste(readLines(pubkey), collapse="\n")
          if (grepl("CERTIFICATE", txt)) openssl::read_cert(pubkey)$pubkey
          else openssl::read_pubkey(pubkey)
        }

  data = readBin(pdf, "raw", file.info(pdf)$size)
  sigOK = tryCatch(openssl::signature_verify(data, sig, hash = openssl::sha256, pubkey = pub),
                   error = function(e) FALSE)
  shaOK = isTRUE(as.character(openssl::sha256(data)) == getf("sha256"))
  valid = isTRUE(sigOK) && shaOK

  cat(sprintf("Verification of '%s'\n", basename(pdf)))
  cat(sprintf("  signer        : %s%s\n", getf("signer"),
              if (nzchar(getf("role"))) paste0("  (", getf("role"), ")") else ""))
  cat(sprintf("  signed at     : %s\n", getf("datetime")))
  cat(sprintf("  sha256 match  : %s\n", shaOK))
  cat(sprintf("  signature     : %s\n", if (isTRUE(sigOK)) "VALID" else "INVALID"))
  cat(sprintf("  >>> RESULT    : %s\n", if (valid) "VERIFIED" else "NOT VERIFIED"))
  invisible(valid)
}
