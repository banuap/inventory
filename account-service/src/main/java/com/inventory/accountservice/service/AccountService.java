package com.inventory.accountservice.service;

import com.inventory.accountservice.dto.AccountDto;
import com.inventory.accountservice.entity.Account;
import com.inventory.accountservice.entity.AccountType;
import com.inventory.accountservice.repository.AccountRepository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.util.List;
import java.util.Optional;
import java.util.stream.Collectors;

@Service
public class AccountService {

    @Autowired
    private AccountRepository accountRepository;

    public List<AccountDto> getAllAccounts() {
        return accountRepository.findAll().stream()
                .map(this::convertToDto)
                .collect(Collectors.toList());
    }

    public Optional<AccountDto> getAccountById(Long id) {
        return accountRepository.findById(id)
                .map(this::convertToDto);
    }

    public Optional<AccountDto> getAccountByEmail(String email) {
        return accountRepository.findByEmail(email)
                .map(this::convertToDto);
    }

    public List<AccountDto> getAccountsByType(AccountType accountType) {
        return accountRepository.findByAccountType(accountType).stream()
                .map(this::convertToDto)
                .collect(Collectors.toList());
    }

    public List<AccountDto> searchAccountsByName(String name) {
        return accountRepository.findByNameContainingIgnoreCase(name).stream()
                .map(this::convertToDto)
                .collect(Collectors.toList());
    }

    public AccountDto createAccount(AccountDto accountDto) {
        if (accountRepository.existsByEmail(accountDto.getEmail())) {
            throw new RuntimeException("Account with email " + accountDto.getEmail() + " already exists");
        }

        Account account = convertToEntity(accountDto);
        Account savedAccount = accountRepository.save(account);
        return convertToDto(savedAccount);
    }

    public Optional<AccountDto> updateAccount(Long id, AccountDto accountDto) {
        return accountRepository.findById(id)
                .map(existingAccount -> {
                    // Check if email is being changed and if new email already exists
                    if (!existingAccount.getEmail().equals(accountDto.getEmail()) &&
                        accountRepository.existsByEmail(accountDto.getEmail())) {
                        throw new RuntimeException("Account with email " + accountDto.getEmail() + " already exists");
                    }

                    existingAccount.setName(accountDto.getName());
                    existingAccount.setAddress(accountDto.getAddress());
                    existingAccount.setEmail(accountDto.getEmail());
                    existingAccount.setAccountType(accountDto.getAccountType());

                    Account updatedAccount = accountRepository.save(existingAccount);
                    return convertToDto(updatedAccount);
                });
    }

    public boolean deleteAccount(Long id) {
        if (accountRepository.existsById(id)) {
            accountRepository.deleteById(id);
            return true;
        }
        return false;
    }

    private AccountDto convertToDto(Account account) {
        AccountDto dto = new AccountDto();
        dto.setId(account.getId());
        dto.setName(account.getName());
        dto.setAddress(account.getAddress());
        dto.setEmail(account.getEmail());
        dto.setAccountType(account.getAccountType());
        return dto;
    }

    private Account convertToEntity(AccountDto dto) {
        Account account = new Account();
        account.setName(dto.getName());
        account.setAddress(dto.getAddress());
        account.setEmail(dto.getEmail());
        account.setAccountType(dto.getAccountType());
        return account;
    }
}