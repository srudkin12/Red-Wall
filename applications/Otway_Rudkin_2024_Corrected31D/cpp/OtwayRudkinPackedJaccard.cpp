// Exact packed-signature Jaccard engine for Otway-Rudkin P1.4-B.
// Application-level performance helper: the generic TDABM framework is unchanged.
//
// [[Rcpp::plugins(cpp11)]]
// [[Rcpp::plugins(openmp)]]
#include <Rcpp.h>
#include <vector>
#include <cstdint>
#include <cstring>
#include <algorithm>

#ifdef _OPENMP
#include <omp.h>
#endif

using namespace Rcpp;

static inline void counts_for_pair(
    const unsigned char* a,
    const unsigned char* b,
    std::size_t nbytes,
    std::uint64_t& inter,
    std::uint64_t& uni
) {
    inter = 0u;
    uni = 0u;
    std::size_t q = 0u;
    for (; q + sizeof(std::uint64_t) <= nbytes; q += sizeof(std::uint64_t)) {
        std::uint64_t x, y;
        std::memcpy(&x, a + q, sizeof(std::uint64_t));
        std::memcpy(&y, b + q, sizeof(std::uint64_t));
        inter += static_cast<std::uint64_t>(__builtin_popcountll(x & y));
        uni   += static_cast<std::uint64_t>(__builtin_popcountll(x | y));
    }
    for (; q < nbytes; ++q) {
        const unsigned int x = static_cast<unsigned int>(a[q]);
        const unsigned int y = static_cast<unsigned int>(b[q]);
        inter += static_cast<std::uint64_t>(__builtin_popcount(x & y));
        uni   += static_cast<std::uint64_t>(__builtin_popcount(x | y));
    }
}

static std::vector<unsigned char> flatten_raw_signatures(
    const List& signatures,
    std::size_t expected_bytes
) {
    const int m = signatures.size();
    std::vector<unsigned char> flat(static_cast<std::size_t>(m) * expected_bytes);
    for (int i = 0; i < m; ++i) {
        RawVector x = signatures[i];
        if (static_cast<std::size_t>(x.size()) != expected_bytes) {
            stop("Packed signature byte length mismatch in exact stability engine.");
        }
        std::copy(x.begin(), x.end(),
                  flat.begin() + static_cast<std::size_t>(i) * expected_bytes);
    }
    return flat;
}

// [[Rcpp::export]]
bool or24_exact_stability_openmp_enabled() {
#ifdef _OPENMP
    return true;
#else
    return false;
#endif
}

// [[Rcpp::export]]
int or24_exact_stability_openmp_max_threads() {
#ifdef _OPENMP
    return omp_get_max_threads();
#else
    return 1;
#endif
}

// [[Rcpp::export]]
Rcpp::List or24_exact_packed_pairwise_scores(
    Rcpp::List cover_signatures,
    Rcpp::List landmark_signatures,
    int cover_bits,
    int landmark_bits,
    int nthreads = 1
) {
    const int m = cover_signatures.size();
    if (m < 2) stop("At least two signatures are required.");
    if (landmark_signatures.size() != m) stop("Cover/landmark signature count mismatch.");
    if (cover_bits < 1 || landmark_bits < 1) stop("Bit counts must be positive.");
    if (nthreads < 1) stop("nthreads must be >= 1.");
#ifndef _OPENMP
    if (nthreads > 1) stop("OpenMP is unavailable; refusing an unintended serial run.");
#endif

    const std::size_t cover_bytes =
        static_cast<std::size_t>((static_cast<long long>(cover_bits) + 7LL) / 8LL);
    const std::size_t landmark_bytes =
        static_cast<std::size_t>((static_cast<long long>(landmark_bits) + 7LL) / 8LL);

    const std::vector<unsigned char> cover =
        flatten_raw_signatures(cover_signatures, cover_bytes);
    const std::vector<unsigned char> landmark =
        flatten_raw_signatures(landmark_signatures, landmark_bytes);

    const std::size_t npairs =
        static_cast<std::size_t>(m) * static_cast<std::size_t>(m - 1) / 2u;
    std::vector<double> cover_scores(npairs);
    std::vector<double> landmark_scores(npairs);

    std::vector<std::size_t> offsets(static_cast<std::size_t>(m), 0u);
    for (int i = 0; i < m - 1; ++i) {
        offsets[static_cast<std::size_t>(i)] =
            static_cast<std::size_t>(i) *
            static_cast<std::size_t>(2 * m - i - 1) / 2u;
    }

#ifdef _OPENMP
    omp_set_dynamic(0);
#pragma omp parallel for schedule(dynamic) num_threads(nthreads)
#endif
    for (int i = 0; i < m - 1; ++i) {
        const unsigned char* ci =
            cover.data() + static_cast<std::size_t>(i) * cover_bytes;
        const unsigned char* li =
            landmark.data() + static_cast<std::size_t>(i) * landmark_bytes;

        for (int j = i + 1; j < m; ++j) {
            const std::size_t out_idx =
                offsets[static_cast<std::size_t>(i)] +
                static_cast<std::size_t>(j - i - 1);

            const unsigned char* cj =
                cover.data() + static_cast<std::size_t>(j) * cover_bytes;
            const unsigned char* lj =
                landmark.data() + static_cast<std::size_t>(j) * landmark_bytes;

            std::uint64_t inter = 0u, uni = 0u;
            counts_for_pair(ci, cj, cover_bytes, inter, uni);
            cover_scores[out_idx] =
                (uni == 0u) ? 1.0 :
                static_cast<double>(inter) / static_cast<double>(uni);

            inter = 0u; uni = 0u;
            counts_for_pair(li, lj, landmark_bytes, inter, uni);
            landmark_scores[out_idx] =
                (uni == 0u) ? 1.0 :
                static_cast<double>(inter) / static_cast<double>(uni);
        }
    }

    return List::create(
        Named("cover") = wrap(cover_scores),
        Named("landmark") = wrap(landmark_scores)
    );
}
