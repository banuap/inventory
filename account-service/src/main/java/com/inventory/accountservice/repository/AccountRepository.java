package com.inventory.accountservice.repository;

import com.inventory.accountservice.entity.Account;
import com.inventory.accountservice.entity.AccountType;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import java.util.List;
import java.util.Optional;

@Repository
public interface AccountRepository extends JpaRepository<Account, Long> {
    
    Optional<Account> findByEmail(String email);
    
    List<Account> findByAccountType(AccountType accountType);
    
    List<Account> findByNameContainingIgnoreCase(String name);
    
    boolean existsByEmail(String email);
}