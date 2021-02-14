rm(list = ls())


if (!requireNamespace(c('ape','phangorn','seqinr'))) {
  install.packages(c('ape','phangorn','seqinr'))
}

library(ape)
library(phangorn)
library(seqinr)
library(ggtree)
library(ggplot2)
library(patchwork)

test_dna = read.dna('test.fasta', format = 'fasta')
test_phyDat = phyDat(test_dna, type = 'DNA', levels = NULL) # 将DNA序列转换成数字

# 模型评估
mt = modelTest(test_phyDat)
print(mt)

# 计算距离
dna_dist = dist.ml(test_phyDat,model = 'JC69')

# NJ树
tree_nj = NJ(dna_dist)
write.tree()
p_nj = ggtree(tree_nj, layout="radial") +
  geom_tiplab(size = 3, color = 'red')+
  labs(title = 'Neighbor Joining Tree')

p_nj

# UPGMA树
tree_upgma = upgma(dna_dist)
p_UPGMA = ggtree(tree_upgma, layout = 'fan') +
  geom_tiplab(size = 3, color = 'red')+
  labs(title = 'UPGMA Tree')
p_UPGMA

# 最大简约树
parsimony(tree_upgma, data = test_phyDat)
parsimony(tree_nj, data = test_phyDat)
test_optim = optim.parsimony(tree_nj, data = test_phyDat)
tree_peatchet = pratchet(test_phyDat)
p_Maximum_Parsimony = ggtree(tree_peatchet,layout="radial") +
  geom_tiplab(size = 3, color = 'blue')+
  labs(title = 'Maximum Parsimony Tree')
p_Maximum_Parsimony

# 最大似然法
tree_fit = pml(tree_nj, data = test_phyDat)
print(tree_fit)
fitJC = optim.pml(tree_fit, model = 'JC', rearrangement = 'stochastic')
logLik(fitJC)
tree_bs = bootstrap.pml(fitJC, bs = 100, optNni = T,
                        multicore = F, 
                        control = pml.control(trace = 0))
tree_ml_bootstrap = plotBS(midpoint(fitJC$tree), tree_bs, p = 50, type = 'p')

p_Maximum_Likelihood = ggtree(tree_ml_bootstrap, layout = 'fan') +
  geom_tiplab(size = 3, color = 'purple')+
  labs(title = 'Maximum Likelihood Tree')
p_Maximum_Likelihood

p_all = p_nj + p_UPGMA + p_Maximum_Parsimony + p_Maximum_Likelihood +
  plot_layout(ncol = 2)
ggsave(p_all, filename = 'figures/all.pdf', width = 8, height = 8)