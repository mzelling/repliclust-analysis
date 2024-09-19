import numpy as np
from scipy.stats import norm


def compute_b(t, center1, center2, cov1, cov2):
    assert (0 <= t) and (t <= 1)
    return np.linalg.solve(t*cov1 + (1-t)*cov2, center2-center1)


def quadr_form(t, center1, center2, cov1, cov2):
    b = compute_b(t, center1, center2, cov1, cov2)
    return (t**2)*np.sum(b * (cov1 @ b)) - ((1-t)**2)*np.sum(b * (cov2 @ b))


def find_t(center1, center2, cov1, cov2, t_init=0.5, tol=1e-5,
           max_iter=50):
    t = t_init; t_min = 0; t_max = 1
    z = quadr_form(t, center1, center2, cov1, cov2)
    iter_count = 0
    while (np.abs(z) > tol) and (iter_count <= max_iter):
        if z > tol:
            # need to reduce t
            t_max = t
            t = (t_min + t_max) / 2
        elif z < tol:
            # need to increase t
            t_min = t
            t = (t_min + t_max) / 2
        z = quadr_form(t, center1, center2, cov1, cov2)
        iter_count+=1
        #print(iter_count, ":", z)
    return t


def compute_marginal_sd(axis, cov):
    return np.sqrt(np.sum(axis * (cov @ axis)))


def compute_q(axis,center1,center2,cov1,cov2):
    return (np.sum(axis * (center2-center1))
            / (compute_marginal_sd(axis,cov1)
                + compute_marginal_sd(axis,cov2)))
    

def compute_exact_q(center1, center2, cov1, cov2):
    t_star = find_t(center1, center2, cov1, cov2)
    b_star = compute_b(t_star, center1, center2, cov1, cov2)
    return compute_q(b_star,center1,center2,cov1,cov2)


def compute_lda_q(center1, center2, cov1, cov2):
    l_axis = np.linalg.solve((cov1 + cov2)/2, center2-center1)
    return compute_q(l_axis,center1,center2,cov1,cov2)


def compute_heuristic_q(center1, center2, cov1, cov2):
    delta = center2-center1
    return compute_q(delta,center1,center2,cov1,cov2)


def compute_overlaps(centers, cov, mode='exact'):
    if mode=='exact':
        q_fun = compute_exact_q
    elif mode=='lda':
        q_fun = compute_lda_q
    elif mode=='heuristic':
        q_fun = compute_heuristic_q
    
    k = centers.shape[0]
    max_overlaps = np.array(
                    [np.max(
                        [2*(1-norm.cdf(
                                    q_fun(centers[i,:], centers[j,:],
                                          cov[i],cov[j])))
                         for j in range(k) if not j==i]
                         )
                     for i in range(k)]
    )
    min_o = np.min(max_overlaps)
    max_o = np.max(max_overlaps)
    return (min_o, max_o)

def compute_pairwise_overlaps(centers, cov, mode='exact', exclude_self=True):
    """ 
    Compute all pairwise overlaps. Given k cluster centers and 
    covariance matrices, returns either a k-(k-1) matrix or k-by-k
    matrix of all pairwise overlaps. If exclude_self=True, the column
    dimensions is k-1 because each row excludes a cluster's overlap with
    itself. If exclude_self=False, the output matrix has dimensions
    k-by-k and puts the value of a cluster's self-overlap at 1.
    """
    if mode=='exact':
        q_fun = compute_exact_q
    elif mode=='lda':
        q_fun = compute_lda_q
    elif mode=='heuristic':
        q_fun = compute_heuristic_q
    
    k = centers.shape[0]
    if exclude_self:
        pw_overlaps = np.array(
            [
                [
                    2*(1-norm.cdf(q_fun(centers[i,:], centers[j,:],cov[i],cov[j])))
                        for j in range(k) if not j==i
                ]
                for i in range(k)
            ]
        )
    else:
        pw_overlaps = np.array(
            [
                [
                    2*(1-norm.cdf(q_fun(centers[i,:], centers[j,:],cov[i],cov[j])))
                    if j != i else 1
                    for j in range(k)
                ]
                for i in range(k)
            ]
        )


    return pw_overlaps